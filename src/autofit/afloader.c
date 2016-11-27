/***************************************************************************/
/*                                                                         */
/*  afloader.c                                                             */
/*                                                                         */
/*    Auto-fitter glyph loading routines (body).                           */
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
#include "afloader.h"
#include "afhints.h"
#include "aferrors.h"
#include "afmodule.h"
#include "afpic.h"


  /* Initialize glyph loader. */

  FT_LOCAL_DEF( void )
  af_loader_init( AF_Loader      loader,
                  AF_GlyphHints  hints )
  {
    FT_ZERO( loader );

    loader->hints = hints;
  }


  /* Reset glyph loader and compute globals if necessary. */

  FT_LOCAL_DEF( FT_Error )
  af_loader_reset( AF_Loader  loader,
                   AF_Module  module,
                   FT_Face    face )
  {
    FT_Error  error = FT_Err_Ok;


    loader->face    = face;
    loader->globals = (AF_FaceGlobals)face->autohint.data;

    if ( !loader->globals )
    {
      error = af_face_globals_new( face, &loader->globals, module );
      if ( !error )
      {
        face->autohint.data =
          (FT_Pointer)loader->globals;
        face->autohint.finalizer =
          (FT_Generic_Finalizer)af_face_globals_free;
      }
    }

    return error;
  }


  /* Finalize glyph loader. */

  FT_LOCAL_DEF( void )
  af_loader_done( AF_Loader  loader )
  {
    loader->face    = NULL;
    loader->globals = NULL;
    loader->hints   = NULL;
  }


#define af_intToFixed( i ) \
          ( (FT_Fixed)( (FT_UInt32)(i) << 16 ) )
#define af_fixedToInt( x ) \
          ( (FT_Short)( ( (FT_UInt32)(x) + 0x8000U ) >> 16 ) )


  FT_LOCAL( void )
  af_loader_embolden_glyph_in_slot( AF_Loader  loader,
                                    FT_Face    face )
  {
    AF_FaceGlobals globals  = loader->globals;
    FT_Fixed       darken_x, darken_y;
    FT_Fixed       em_size  = af_intToFixed( face->units_per_EM );
    FT_Fixed       em_ratio = FT_DivFix( af_intToFixed( 1000 ), em_size );


    darken_x = FT_DivFix( FT_MulFix( af_intToFixed( globals->darken_x ),
                                     face->size->metrics.x_scale ),
                          em_ratio );

    darken_y = FT_DivFix( FT_MulFix( af_intToFixed( globals->darken_y ),
                                     face->size->metrics.y_scale ),
                          em_ratio );


    FT_Outline_EmboldenXY( &face->glyph->outline,
                           af_fixedToInt( darken_x ),
                           af_fixedToInt( darken_y ) );

  }


  /* Load the glyph at index into the current slot of a face and hint it. */

  FT_LOCAL_DEF( FT_Error )
  af_loader_load_glyph( AF_Loader  loader,
                        AF_Module  module,
                        FT_Face    face,
                        FT_UInt    glyph_index,
                        FT_Int32   load_flags )
  {
    FT_Error  error;

    FT_Size           size     = face->size;
    FT_GlyphSlot      slot     = face->glyph;
    FT_Slot_Internal  internal = slot->internal;
    FT_GlyphLoader    gloader  = internal->loader;

    AF_GlyphHints          hints         = loader->hints;
    AF_ScalerRec           scaler;
    AF_StyleMetrics        style_metrics;
    FT_UInt                style_options = AF_STYLE_NONE_DFLT;
    AF_StyleClass          style_class;
    AF_WritingSystemClass  writing_system_class;

#ifdef FT_CONFIG_OPTION_PIC
    AF_FaceGlobals  globals = loader->globals;
#endif


    if ( !size )
      return FT_THROW( Invalid_Size_Handle );

    FT_ZERO( &scaler );

    /*
     *  TODO: This code currently doesn't support fractional advance widths,
     *  i.e., placing hinted glyphs at anything other than integer
     *  x-positions.  This is only relevant for the warper code, which
     *  scales and shifts glyphs to optimize blackness of stems (hinting on
     *  the x-axis by nature places things on pixel integers, hinting on the
     *  y-axis only, i.e., LIGHT mode, doesn't touch the x-axis).  The delta
     *  values of the scaler would need to be adjusted.
     */
    scaler.face    = face;
    scaler.x_scale = size->metrics.x_scale;
    scaler.x_delta = 0;
    scaler.y_scale = size->metrics.y_scale;
    scaler.y_delta = 0;

    scaler.render_mode = FT_LOAD_TARGET_MODE( load_flags );
    scaler.flags       = 0;

    /* note that the fallback style can't be changed anymore */
    /* after the first call of `ta_loader_load_glyph'        */
    error = af_loader_reset( loader, module, face );
    if ( error )
      goto Exit;

#ifdef FT_OPTION_AUTOFIT2
    /* XXX: undocumented hook to activate the latin2 writing system. */
    if ( load_flags & ( 1UL << 20 ) )
      style_options = AF_STYLE_LTN2_DFLT;
#endif

    /*
     *  Glyphs (really code points) are assigned to scripts.  Script
     *  analysis is done lazily: For each glyph that passes through here,
     *  the corresponding script analyzer is called, but returns immediately
     *  if it has been run already.
     */
    error = af_face_globals_get_metrics( loader->globals, glyph_index,
                                         style_options, &style_metrics );
    if ( error )
      goto Exit;

    style_class          = style_metrics->style_class;
    writing_system_class =
      AF_WRITING_SYSTEM_CLASSES_GET[style_class->writing_system];

    loader->metrics = style_metrics;

    if ( writing_system_class->style_metrics_scale )
      writing_system_class->style_metrics_scale( style_metrics, &scaler );
    else
      style_metrics->scaler = scaler;

    if ( writing_system_class->style_hints_init )
    {
      error = writing_system_class->style_hints_init( hints,
                                                      style_metrics );
      if ( error )
        goto Exit;
    }

    /*
     *  Do the main work of `af_loader_load_glyph'.  Note that we never have
     *  to deal with composite glyphs as those get loaded into
     *  FT_GLYPH_FORMAT_OUTLINE by the recursed `FT_Load_Glyph' function.
     *  In the rare cases where FT_LOAD_NO_RECURSE is set, it implies
     *  FT_LOAD_NO_SCALE and as such the auto-hinter is never called.
     */
    load_flags |=  FT_LOAD_NO_SCALE         |
                   FT_LOAD_IGNORE_TRANSFORM |
                   FT_LOAD_LINEAR_DESIGN;
    load_flags &= ~FT_LOAD_RENDER;

    error = FT_Load_Glyph( face, glyph_index, load_flags );
    if ( error )
      goto Exit;

    /*
     *  Apply stem darkening (emboldening) here before hints are applied to
     *  the outline.  Glyphs are scaled down proportionally to the
     *  emboldening so that curve points don't fall outside their
     *  precomputed blue zones.
     *
     *  Any emboldening done by the font driver (e.g., the CFF driver)
     *  doesn't reach here because the autohinter loads the unprocessed
     *  glyphs in font units for analysis (functions `af_*_metrics_init_*')
     *  and then above to prepare it for the rasterizers by itself,
     *  independently of the font driver.  So emboldening must be done here,
     *  within the autohinter.
     *
     *  All glyphs to be autohinted pass through here one by one.  The
     *  standard widths can therefore change from one glyph to the next,
     *  depending on what script a glyph is assigned to (each script has its
     *  own set of standard widths and other metrics).  The darkening amount
     *  must therefore be recomputed for each size and
     *  `standard_{vertical,horizontal}_width' change.
     *
     *  Ignore errors and carry on without emboldening.
     *
     */

    /* stem darkening only works well in `light' mode */
    if ( scaler.render_mode == FT_RENDER_MODE_LIGHT    &&
         ( !face->internal->no_stem_darkening        ||
           ( face->internal->no_stem_darkening < 0 &&
             !module->no_stem_darkening            ) ) )
      af_loader_embolden_glyph_in_slot( loader, face );


    loader->transformed = internal->glyph_transformed;
    if ( loader->transformed )
    {
      FT_Matrix  inverse;


      loader->trans_matrix = internal->glyph_matrix;
      loader->trans_delta  = internal->glyph_delta;

      inverse = loader->trans_matrix;
      if ( !FT_Matrix_Invert( &inverse ) )
        FT_Vector_Transform( &loader->trans_delta, &inverse );
    }

    switch ( slot->format )
    {
    case FT_GLYPH_FORMAT_OUTLINE:
      /* translate the loaded glyph when an internal transform is needed */
      if ( loader->transformed )
        FT_Outline_Translate( &slot->outline,
                              loader->trans_delta.x,
                              loader->trans_delta.y );

      /* compute original horizontal phantom points */
      /* (and ignore vertical ones)                 */
      loader->pp1.x = hints->x_delta;
      loader->pp1.y = hints->y_delta;
      loader->pp2.x = FT_MulFix( slot->metrics.horiAdvance,
                                 hints->x_scale ) + hints->x_delta;
      loader->pp2.y = hints->y_delta;

      /* be sure to check for spacing glyphs */
      if ( slot->outline.n_points == 0 )
        goto Hint_Metrics;

      /* now load the slot image into the auto-outline */
      /* and run the automatic hinting process         */
      if ( writing_system_class->style_hints_apply )
        writing_system_class->style_hints_apply( glyph_index,
                                                 hints,
                                                 &gloader->base.outline,
                                                 style_metrics );

      /* we now need to adjust the metrics according to the change in */
      /* width/positioning that occurred during the hinting process   */
      if ( scaler.render_mode != FT_RENDER_MODE_LIGHT )
      {
        FT_Pos  old_rsb, old_lsb, new_lsb;
        FT_Pos  pp1x_uh, pp2x_uh;

        AF_AxisHints  axis  = &hints->axis[AF_DIMENSION_HORZ];
        AF_Edge       edge1 = axis->edges;         /* leftmost edge  */
        AF_Edge       edge2 = edge1 +
                              axis->num_edges - 1; /* rightmost edge */


        if ( axis->num_edges > 1 && AF_HINTS_DO_ADVANCE( hints ) )
        {
          old_rsb = loader->pp2.x - edge2->opos;
          /* loader->pp1.x is always zero at this point of time */
          old_lsb = edge1->opos /* - loader->pp1.x */;
          new_lsb = edge1->pos;

          /* remember unhinted values to later account */
          /* for rounding errors                       */
          pp1x_uh = new_lsb    - old_lsb;
          pp2x_uh = edge2->pos + old_rsb;

          /* prefer too much space over too little space */
          /* for very small sizes                        */

          if ( old_lsb < 24 )
            pp1x_uh -= 8;

          if ( old_rsb < 24 )
            pp2x_uh += 8;

          loader->pp1.x = FT_PIX_ROUND( pp1x_uh );
          loader->pp2.x = FT_PIX_ROUND( pp2x_uh );

          if ( loader->pp1.x >= new_lsb && old_lsb > 0 )
            loader->pp1.x -= 64;

          if ( loader->pp2.x <= edge2->pos && old_rsb > 0 )
            loader->pp2.x += 64;

          slot->lsb_delta = loader->pp1.x - pp1x_uh;
          slot->rsb_delta = loader->pp2.x - pp2x_uh;
        }
        else
        {
          FT_Pos  pp1x = loader->pp1.x;
          FT_Pos  pp2x = loader->pp2.x;


          loader->pp1.x = FT_PIX_ROUND( pp1x );
          loader->pp2.x = FT_PIX_ROUND( pp2x );

          slot->lsb_delta = loader->pp1.x - pp1x;
          slot->rsb_delta = loader->pp2.x - pp2x;
        }
      }
      else
      {
        FT_Pos  pp1x = loader->pp1.x;
        FT_Pos  pp2x = loader->pp2.x;


        loader->pp1.x = FT_PIX_ROUND( pp1x + hints->xmin_delta );
        loader->pp2.x = FT_PIX_ROUND( pp2x + hints->xmax_delta );

        slot->lsb_delta = loader->pp1.x - pp1x;
        slot->rsb_delta = loader->pp2.x - pp2x;
      }

      break;

    default:
      /* we don't support other formats (yet?) */
      error = FT_THROW( Unimplemented_Feature );
    }

  Hint_Metrics:
    {
      FT_BBox    bbox;
      FT_Vector  vvector;


      vvector.x = slot->metrics.vertBearingX - slot->metrics.horiBearingX;
      vvector.y = slot->metrics.vertBearingY - slot->metrics.horiBearingY;
      vvector.x = FT_MulFix( vvector.x, style_metrics->scaler.x_scale );
      vvector.y = FT_MulFix( vvector.y, style_metrics->scaler.y_scale );

      /* transform the hinted outline if needed */
      if ( loader->transformed )
      {
        FT_Outline_Transform( &gloader->base.outline, &loader->trans_matrix );
        FT_Vector_Transform( &vvector, &loader->trans_matrix );
      }

      /* we must translate our final outline by -pp1.x and compute */
      /* the new metrics                                           */
      if ( loader->pp1.x )
        FT_Outline_Translate( &gloader->base.outline, -loader->pp1.x, 0 );

      FT_Outline_Get_CBox( &gloader->base.outline, &bbox );

      bbox.xMin = FT_PIX_FLOOR( bbox.xMin );
      bbox.yMin = FT_PIX_FLOOR( bbox.yMin );
      bbox.xMax = FT_PIX_CEIL(  bbox.xMax );
      bbox.yMax = FT_PIX_CEIL(  bbox.yMax );

      slot->metrics.width        = bbox.xMax - bbox.xMin;
      slot->metrics.height       = bbox.yMax - bbox.yMin;
      slot->metrics.horiBearingX = bbox.xMin;
      slot->metrics.horiBearingY = bbox.yMax;

      slot->metrics.vertBearingX = FT_PIX_FLOOR( bbox.xMin + vvector.x );
      slot->metrics.vertBearingY = FT_PIX_FLOOR( bbox.yMax + vvector.y );

      /* for mono-width fonts (like Andale, Courier, etc.) we need */
      /* to keep the original rounded advance width; ditto for     */
      /* digits if all have the same advance width                 */
      if ( scaler.render_mode != FT_RENDER_MODE_LIGHT                       &&
           ( FT_IS_FIXED_WIDTH( slot->face )                              ||
             ( af_face_globals_is_digit( loader->globals, glyph_index ) &&
               style_metrics->digits_have_same_width                    ) ) )
      {
        slot->metrics.horiAdvance =
          FT_MulFix( slot->metrics.horiAdvance,
                     style_metrics->scaler.x_scale );

        /* Set delta values to 0.  Otherwise code that uses them is */
        /* going to ruin the fixed advance width.                   */
        slot->lsb_delta = 0;
        slot->rsb_delta = 0;
      }
      else
      {
        /* non-spacing glyphs must stay as-is */
        if ( slot->metrics.horiAdvance )
          slot->metrics.horiAdvance = loader->pp2.x - loader->pp1.x;
      }

      slot->metrics.vertAdvance = FT_MulFix( slot->metrics.vertAdvance,
                                             style_metrics->scaler.y_scale );

      slot->metrics.horiAdvance = FT_PIX_ROUND( slot->metrics.horiAdvance );
      slot->metrics.vertAdvance = FT_PIX_ROUND( slot->metrics.vertAdvance );

      slot->format  = FT_GLYPH_FORMAT_OUTLINE;
    }

  Exit:
    return error;
  }


/* END */
