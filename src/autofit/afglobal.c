/***************************************************************************/
/*                                                                         */
/*  afglobal.c                                                             */
/*                                                                         */
/*    Auto-fitter routines to compute global hinting values (body).        */
/*                                                                         */
/*  Copyright 2003-2017 by                                                 */
/*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
/*                                                                         */
/*  This file is part of the FreeType project, and may only be used,       */
/*  modified, and distributed under the terms of the FreeType project      */
/*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
/*  this file you indicate that you have read the license and              */
/*  understand and accept it fully.                                        */
/*                                                                         */
/***************************************************************************/


#include "afglobal.h"
#include "afranges.h"
#include "afshaper.h"
#include FT_INTERNAL_DEBUG_H
#include FT_INTERNAL_CALC_H


  /*************************************************************************/
  /*                                                                       */
  /* The macro FT_COMPONENT is used in trace mode.  It is an implicit      */
  /* parameter of the FT_TRACE() and FT_ERROR() macros, used to print/log  */
  /* messages during execution.                                            */
  /*                                                                       */
#undef  FT_COMPONENT
#define FT_COMPONENT  trace_afglobal


  /* get writing system specific header files */
#undef  WRITING_SYSTEM
#define WRITING_SYSTEM( ws, WS )  /* empty */
#include "afwrtsys.h"

#include "aferrors.h"
#include "afpic.h"


#undef  SCRIPT
#define SCRIPT( s, S, d, h, H, ss )         \
          AF_DEFINE_SCRIPT_CLASS(           \
            af_ ## s ## _script_class,      \
            AF_SCRIPT_ ## S,                \
            af_ ## s ## _uniranges,         \
            af_ ## s ## _nonbase_uniranges, \
            AF_ ## H,                       \
            ss )

#include "afscript.h"


#undef  STYLE
#define STYLE( s, S, d, ws, sc, ss, c )  \
          AF_DEFINE_STYLE_CLASS(         \
            af_ ## s ## _style_class,    \
            AF_STYLE_ ## S,              \
            ws,                          \
            sc,                          \
            ss,                          \
            c )

#include "afstyles.h"


#ifndef FT_CONFIG_OPTION_PIC

#undef  WRITING_SYSTEM
#define WRITING_SYSTEM( ws, WS )               \
          &af_ ## ws ## _writing_system_class,

  FT_LOCAL_ARRAY_DEF( AF_WritingSystemClass )
  af_writing_system_classes[] =
  {

#include "afwrtsys.h"

    NULL  /* do not remove */
  };


#undef  SCRIPT
#define SCRIPT( s, S, d, h, H, ss )   \
          &af_ ## s ## _script_class,

  FT_LOCAL_ARRAY_DEF( AF_ScriptClass )
  af_script_classes[] =
  {

#include "afscript.h"

    NULL  /* do not remove */
  };


#undef  STYLE
#define STYLE( s, S, d, ws, sc, ss, c ) \
          &af_ ## s ## _style_class,

  FT_LOCAL_ARRAY_DEF( AF_StyleClass )
  af_style_classes[] =
  {

#include "afstyles.h"

    NULL  /* do not remove */
  };

#endif /* !FT_CONFIG_OPTION_PIC */


#ifdef FT_DEBUG_LEVEL_TRACE

#undef  STYLE
#define STYLE( s, S, d, ws, sc, ss, c )  #s,

  FT_LOCAL_ARRAY_DEF( char* )
  af_style_names[] =
  {

#include "afstyles.h"

  };

#endif /* FT_DEBUG_LEVEL_TRACE */


  /* Compute the style index of each glyph within a given face. */

  static FT_Error
  af_face_globals_compute_style_coverage( AF_FaceGlobals  globals )
  {
    FT_Error    error;
    FT_Face     face        = globals->face;
    FT_CharMap  old_charmap = face->charmap;
    FT_UShort*  gstyles     = globals->glyph_styles;
    FT_UInt     ss;
    FT_UInt     i;
    FT_UInt     dflt        = ~0U; /* a non-valid value */


    /* the value AF_STYLE_UNASSIGNED means `uncovered glyph' */
    for ( i = 0; i < (FT_UInt)globals->glyph_count; i++ )
      gstyles[i] = AF_STYLE_UNASSIGNED;

    error = FT_Select_Charmap( face, FT_ENCODING_UNICODE );
    if ( error )
    {
      /*
       * Ignore this error; we simply use the fallback style.
       * XXX: Shouldn't we rather disable hinting?
       */
      error = FT_Err_Ok;
      goto Exit;
    }

    /* scan each style in a Unicode charmap */
    for ( ss = 0; AF_STYLE_CLASSES_GET[ss]; ss++ )
    {
      AF_StyleClass       style_class =
                            AF_STYLE_CLASSES_GET[ss];
      AF_ScriptClass      script_class =
                            AF_SCRIPT_CLASSES_GET[style_class->script];
      AF_Script_UniRange  range;


      if ( !script_class->script_uni_ranges )
        continue;

      /*
       *  Scan all Unicode points in the range and set the corresponding
       *  glyph style index.
       */
      if ( style_class->coverage == AF_COVERAGE_DEFAULT )
      {
        if ( (FT_UInt)style_class->script ==
             globals->module->default_script )
          dflt = ss;

        for ( range = script_class->script_uni_ranges;
              range->first != 0;
              range++ )
        {
          FT_ULong  charcode = range->first;
          FT_UInt   gindex;


          gindex = FT_Get_Char_Index( face, charcode );

          if ( gindex != 0                                                &&
               gindex < (FT_ULong)globals->glyph_count                    &&
               ( gstyles[gindex] & AF_STYLE_MASK ) == AF_STYLE_UNASSIGNED )
            gstyles[gindex] = (FT_UShort)ss;

          for (;;)
          {
            charcode = FT_Get_Next_Char( face, charcode, &gindex );

            if ( gindex == 0 || charcode > range->last )
              break;

            if ( gindex < (FT_ULong)globals->glyph_count                    &&
                 ( gstyles[gindex] & AF_STYLE_MASK ) == AF_STYLE_UNASSIGNED )
              gstyles[gindex] = (FT_UShort)ss;
          }
        }

        /* do the same for the script's non-base characters */
        for ( range = script_class->script_uni_nonbase_ranges;
              range->first != 0;
              range++ )
        {
          FT_ULong  charcode = range->first;
          FT_UInt   gindex;


          gindex = FT_Get_Char_Index( face, charcode );

          if ( gindex != 0                                          &&
               gindex < (FT_ULong)globals->glyph_count              &&
               ( gstyles[gindex] & AF_STYLE_MASK ) == (FT_UShort)ss )
            gstyles[gindex] |= AF_NONBASE;

          for (;;)
          {
            charcode = FT_Get_Next_Char( face, charcode, &gindex );

            if ( gindex == 0 || charcode > range->last )
              break;

            if ( gindex < (FT_ULong)globals->glyph_count              &&
                 ( gstyles[gindex] & AF_STYLE_MASK ) == (FT_UShort)ss )
              gstyles[gindex] |= AF_NONBASE;
          }
        }
      }
      else
      {
        /* get glyphs not directly addressable by cmap */
        af_shaper_get_coverage( globals, style_class, gstyles, 0 );
      }
    }

    /* handle the remaining default OpenType features ... */
    for ( ss = 0; AF_STYLE_CLASSES_GET[ss]; ss++ )
    {
      AF_StyleClass  style_class = AF_STYLE_CLASSES_GET[ss];


      if ( style_class->coverage == AF_COVERAGE_DEFAULT )
        af_shaper_get_coverage( globals, style_class, gstyles, 0 );
    }

    /* ... and finally the default OpenType features of the default script */
    af_shaper_get_coverage( globals, AF_STYLE_CLASSES_GET[dflt], gstyles, 1 );

    /* mark ASCII digits */
    for ( i = 0x30; i <= 0x39; i++ )
    {
      FT_UInt  gindex = FT_Get_Char_Index( face, i );


      if ( gindex != 0 && gindex < (FT_ULong)globals->glyph_count )
        gstyles[gindex] |= AF_DIGIT;
    }

  Exit:
    /*
     *  By default, all uncovered glyphs are set to the fallback style.
     *  XXX: Shouldn't we disable hinting or do something similar?
     */
    if ( globals->module->fallback_style != AF_STYLE_UNASSIGNED )
    {
      FT_Long  nn;


      for ( nn = 0; nn < globals->glyph_count; nn++ )
      {
        if ( ( gstyles[nn] & AF_STYLE_MASK ) == AF_STYLE_UNASSIGNED )
        {
          gstyles[nn] &= ~AF_STYLE_MASK;
          gstyles[nn] |= globals->module->fallback_style;
        }
      }
    }

#ifdef FT_DEBUG_LEVEL_TRACE

    FT_TRACE4(( "\n"
                "style coverage\n"
                "==============\n"
                "\n" ));

    for ( ss = 0; AF_STYLE_CLASSES_GET[ss]; ss++ )
    {
      AF_StyleClass  style_class = AF_STYLE_CLASSES_GET[ss];
      FT_UInt        count       = 0;
      FT_Long        idx;


      FT_TRACE4(( "%s:\n", af_style_names[style_class->style] ));

      for ( idx = 0; idx < globals->glyph_count; idx++ )
      {
        if ( ( gstyles[idx] & AF_STYLE_MASK ) == style_class->style )
        {
          if ( !( count % 10 ) )
            FT_TRACE4(( " " ));

          FT_TRACE4(( " %d", idx ));
          count++;

          if ( !( count % 10 ) )
            FT_TRACE4(( "\n" ));
        }
      }

      if ( !count )
        FT_TRACE4(( "  (none)\n" ));
      if ( count % 10 )
        FT_TRACE4(( "\n" ));
    }

#endif /* FT_DEBUG_LEVEL_TRACE */

    FT_Set_Charmap( face, old_charmap );
    return error;
  }


  FT_LOCAL_DEF( FT_Error )
  af_face_globals_new( FT_Face          face,
                       AF_FaceGlobals  *aglobals,
                       AF_Module        module )
  {
    FT_Error        error;
    FT_Memory       memory;
    AF_FaceGlobals  globals = NULL;


    memory = face->memory;

    /* we allocate an AF_FaceGlobals structure together */
    /* with the glyph_styles array                      */
    if ( FT_ALLOC( globals,
                   sizeof ( *globals ) +
                     (FT_ULong)face->num_glyphs * sizeof ( FT_UShort ) ) )
      goto Exit;

    globals->face                      = face;
    globals->glyph_count               = face->num_glyphs;
    /* right after the globals structure come the glyph styles */
    globals->glyph_styles              = (FT_UShort*)( globals + 1 );
    globals->module                    = module;
    globals->darken_x                  = 0;
    globals->darken_y                  = 0;

#ifdef FT_CONFIG_OPTION_USE_HARFBUZZ
    globals->hb_font = hb_ft_font_create( face, NULL );
    globals->hb_buf  = hb_buffer_create();
#endif

    error = af_face_globals_compute_style_coverage( globals );
    if ( error )
    {
      af_face_globals_free( globals );
      globals = NULL;
    }
    else
      globals->increase_x_height = AF_PROP_INCREASE_X_HEIGHT_MAX;

  Exit:
    *aglobals = globals;
    return error;
  }


  FT_LOCAL_DEF( void )
  af_face_globals_free( AF_FaceGlobals  globals )
  {
    if ( globals )
    {
      FT_Memory  memory = globals->face->memory;
      FT_UInt    nn;


      for ( nn = 0; nn < AF_STYLE_MAX; nn++ )
      {
        if ( globals->metrics[nn] )
        {
          AF_StyleClass          style_class =
            AF_STYLE_CLASSES_GET[nn];
          AF_WritingSystemClass  writing_system_class =
            AF_WRITING_SYSTEM_CLASSES_GET[style_class->writing_system];


          if ( writing_system_class->style_metrics_done )
            writing_system_class->style_metrics_done( globals->metrics[nn] );

          FT_FREE( globals->metrics[nn] );
        }
      }

#ifdef FT_CONFIG_OPTION_USE_HARFBUZZ
      hb_font_destroy( globals->hb_font );
      hb_buffer_destroy( globals->hb_buf );
#endif

      /* no need to free `globals->glyph_styles'; */
      /* it is part of the `globals' array        */
      FT_FREE( globals );
    }
  }


  FT_LOCAL_DEF( FT_Error )
  af_face_globals_get_metrics( AF_FaceGlobals    globals,
                               FT_UInt           gindex,
                               FT_UInt           options,
                               AF_StyleMetrics  *ametrics )
  {
    AF_StyleMetrics  metrics = NULL;

    AF_Style               style = (AF_Style)options;
    AF_WritingSystemClass  writing_system_class;
    AF_StyleClass          style_class;

    FT_Error  error = FT_Err_Ok;


    if ( gindex >= (FT_ULong)globals->glyph_count )
    {
      error = FT_THROW( Invalid_Argument );
      goto Exit;
    }

    /* if we have a forced style (via `options'), use it, */
    /* otherwise look into `glyph_styles' array           */
    if ( style == AF_STYLE_NONE_DFLT || style + 1 >= AF_STYLE_MAX )
      style = (AF_Style)( globals->glyph_styles[gindex] &
                          AF_STYLE_UNASSIGNED           );

    style_class          = AF_STYLE_CLASSES_GET[style];
    writing_system_class = AF_WRITING_SYSTEM_CLASSES_GET
                             [style_class->writing_system];

    metrics = globals->metrics[style];
    if ( !metrics )
    {
      /* create the global metrics object if necessary */
      FT_Memory  memory = globals->face->memory;


      if ( FT_ALLOC( metrics, writing_system_class->style_metrics_size ) )
        goto Exit;

      metrics->style_class = style_class;
      metrics->globals     = globals;

      if ( writing_system_class->style_metrics_init )
      {
        error = writing_system_class->style_metrics_init( metrics,
                                                          globals->face );
        if ( error )
        {
          if ( writing_system_class->style_metrics_done )
            writing_system_class->style_metrics_done( metrics );

          FT_FREE( metrics );
          goto Exit;
        }
      }

      globals->metrics[style] = metrics;
    }

    /* We now have the standard widths computed, so we can compute the
     * amount of darkening that a glyph should get at the current size.
     * Ignore errors and carry on without darkening. */
    af_face_globals_update_darkening( globals, metrics, writing_system_class );

  Exit:
    *ametrics = metrics;

    return error;
  }


  FT_LOCAL( FT_Error )
  af_face_globals_update_darkening( AF_FaceGlobals         globals,
                                    AF_StyleMetrics        style_metrics,
                                    AF_WritingSystemClass  writing_system_class )
  {
    FT_Error               error = FT_Err_Ok;
    FT_Face                face  = globals->face;
    FT_Pos                 stdVW = 0;
    FT_Pos                 stdHW = 0;


    /* Skip stem darkening for broken fonts. */
    if ( !face->units_per_EM )
    {
      error = FT_Err_Corrupted_Font_Header;
      goto Exit;
    }

    /*
     *  We depend on the writing system (script analyzers) to supply
     *  standard widths for the script of the glyph we are looking at.  If
     *  it can't deliver, stem darkening is disabled.
     */
    if ( !writing_system_class->style_metrics_getstdw )
    {
      error = FT_Err_Unimplemented_Feature;
      goto Exit;
    }

    writing_system_class->style_metrics_getstdw( style_metrics,
                                                 &stdHW,
                                                 &stdVW );

    globals->darken_x = af_face_globals_compute_darkening( globals, stdVW );
    globals->darken_y = af_face_globals_compute_darkening( globals, stdHW );

  Exit:
    return error;
  }


#define af_intToFixed( i ) \
          ( (FT_Fixed)( (FT_UInt32)(i) << 16 ) )
#define af_fixedToInt( x ) \
          ( (FT_Short)( ( (FT_UInt32)(x) + 0x8000U ) >> 16 ) )
#define af_floatToFixed( f ) \
          ( (FT_Fixed)( (f) * 65536.0 + 0.5 ) )


  /*
   * Compute amount of font units the face should be emboldened by, in
   * analogy to the CFF driver's `cf2_computeDarkening' function.  See there
   * for details of the algorithm.
   *
   * XXX: Currently a crude adaption of the original algorithm.  Do better?
   */
  FT_LOCAL( FT_Int32 )
  af_face_globals_compute_darkening( AF_FaceGlobals globals,
                                     FT_Pos         standard_width )
  {
    AF_Module  module = globals->module;
    FT_Face    face = globals->face;
    FT_UShort  units_per_EM;
    FT_Fixed   ppem, em_ratio;
    FT_Fixed   stem_width, stem_width_per_1000, scaled_stem, darken_amount;
    FT_Int     log_base_2;
    FT_Int     x1, y1, x2, y2, x3, y3, x4, y4;


    ppem         = af_intToFixed( FT_MAX( 4, face->size->metrics.x_ppem ) );
    units_per_EM = face->units_per_EM;

    em_ratio = FT_DivFix( af_intToFixed( 1000 ),
                          af_intToFixed( units_per_EM ) );
    if ( em_ratio < af_floatToFixed( .01 ) )
    {
      /* If something goes wrong, don't embolden. */
      return 0;
    }

    x1 = module->darken_params[0];
    y1 = module->darken_params[1];
    x2 = module->darken_params[2];
    y2 = module->darken_params[3];
    x3 = module->darken_params[4];
    y3 = module->darken_params[5];
    x4 = module->darken_params[6];
    y4 = module->darken_params[7];

    if ( standard_width <= 0 )
    {
      stem_width          = af_intToFixed( 75 ); /* taken from cf2font.c */
      stem_width_per_1000 = stem_width;
    }
    else
    {
      stem_width          = af_intToFixed( standard_width );
      stem_width_per_1000 = FT_MulFix( stem_width, em_ratio );
    }

    log_base_2 = FT_MSB( (FT_UInt32)stem_width_per_1000 ) +
                 FT_MSB( (FT_UInt32)ppem );

    if ( log_base_2 >= 46 )
    {
      /* possible overflow */
      scaled_stem = af_intToFixed( x4 );
    }
    else
      scaled_stem = FT_MulFix( stem_width_per_1000, ppem );

    /* now apply the darkening parameters */
    if ( scaled_stem < af_intToFixed( x1 ) )
      darken_amount = FT_DivFix( af_intToFixed( y1 ), ppem );

    else if ( scaled_stem < af_intToFixed( x2 ) )
    {
      FT_Int  xdelta = x2 - x1;
      FT_Int  ydelta = y2 - y1;
      FT_Int  x      = stem_width_per_1000 -
                       FT_DivFix( af_intToFixed( x1 ), ppem );


      if ( !xdelta )
        goto Try_x3;

      darken_amount = FT_MulDiv( x, ydelta, xdelta ) +
                      FT_DivFix( af_intToFixed( y1 ), ppem );
    }

    else if ( scaled_stem < af_intToFixed( x3 ) )
    {
    Try_x3:
      {
        FT_Int  xdelta = x3 - x2;
        FT_Int  ydelta = y3 - y2;
        FT_Int  x      = stem_width_per_1000 -
                         FT_DivFix( af_intToFixed( x2 ), ppem );


        if ( !xdelta )
          goto Try_x4;

        darken_amount = FT_MulDiv( x, ydelta, xdelta ) +
                        FT_DivFix( af_intToFixed( y2 ), ppem );
      }
    }

    else if ( scaled_stem < af_intToFixed( x4 ) )
    {
    Try_x4:
      {
        FT_Int  xdelta = x4 - x3;
        FT_Int  ydelta = y4 - y3;
        FT_Int  x      = stem_width_per_1000 -
                         FT_DivFix( af_intToFixed( x3 ), ppem );


        if ( !xdelta )
          goto Use_y4;

        darken_amount = FT_MulDiv( x, ydelta, xdelta ) +
                        FT_DivFix( af_intToFixed( y3 ), ppem );
      }
    }

    else
    {
    Use_y4:
      darken_amount = FT_DivFix( af_intToFixed( y4 ), ppem );
    }

    /* Convert darken_amount from per 1000 em to true character space. */
    return af_fixedToInt( FT_DivFix( darken_amount, em_ratio ) );
  }

  FT_LOCAL_DEF( FT_Bool )
  af_face_globals_is_digit( AF_FaceGlobals  globals,
                            FT_UInt         gindex )
  {
    if ( gindex < (FT_ULong)globals->glyph_count )
      return (FT_Bool)( globals->glyph_styles[gindex] & AF_DIGIT );

    return (FT_Bool)0;
  }


/* END */
