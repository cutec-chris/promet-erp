{%MainUnit sane.pas}
// included by sane.pas

{  saneopt.h defines several option NAMEs, TITLEs and DESCs
   that are (or should be) used by several backends.

   All well known options should be listed there. But this does
   not mean that all options that are listed there are well known options.
   To find out if an option is a well known option and how well known
   options have to be defined please take a look at the sane standard!!!
   }

{  saneoptsh.inc is a partial Pascal translation of saneopt.h
   It contains only the definitions of option NAMEs from saneopt.h
   }

const
// This _must_ be the first option (index 0):
 SANE_NAME_NUM_OPTIONS		          = '';	// never settable
 SANE_NAME_PREVIEW		          = 'preview';
 SANE_NAME_GRAY_PREVIEW		          = 'preview-in-gray';
 SANE_NAME_BIT_DEPTH		          = 'depth';
 SANE_NAME_SCAN_MODE		          = 'mode';
 SANE_NAME_SCAN_SPEED		          = 'speed';
 SANE_NAME_SCAN_SOURCE		          = 'source';
 SANE_NAME_BACKTRACK		          = 'backtrack';
{ Most user-interfaces will let the user specify the scan area as the
   top-left corner and the width/height of the scan area.  The reason
   the backend interface uses the top-left/bottom-right corner is so
   that the scan area values can be properly constraint independent of
   any other option value.  }
 SANE_NAME_SCAN_TL_X		          = 'tl-x';
 SANE_NAME_SCAN_TL_Y		          = 'tl-y';
 SANE_NAME_SCAN_BR_X		          = 'br-x';
 SANE_NAME_SCAN_BR_Y		          = 'br-y';
 SANE_NAME_SCAN_RESOLUTION	          = 'resolution';
 SANE_NAME_SCAN_X_RESOLUTION	          = 'resolution';
 SANE_NAME_SCAN_Y_RESOLUTION	          = 'y-resolution';
 SANE_NAME_CUSTOM_GAMMA		          = 'custom-gamma';
 SANE_NAME_GAMMA_VECTOR		          = 'gamma-table';
 SANE_NAME_GAMMA_VECTOR_R	          = 'red-gamma-table';
 SANE_NAME_GAMMA_VECTOR_G	          = 'green-gamma-table';
 SANE_NAME_GAMMA_VECTOR_B	          = 'blue-gamma-table';
 SANE_NAME_BRIGHTNESS		          = 'brightness';
 SANE_NAME_CONTRAST		          = 'contrast';
 SANE_NAME_GRAIN_SIZE		          = 'grain';
 SANE_NAME_HALFTONE		          = 'halftoning';
 SANE_NAME_BLACK_LEVEL                    = 'black-level';
 SANE_NAME_WHITE_LEVEL                    = 'white-level';
 SANE_NAME_SHADOW	                  = 'shadow';
 SANE_NAME_SHADOW_R		          = 'shadow-r';
 SANE_NAME_SHADOW_G		          = 'shadow-g';
 SANE_NAME_SHADOW_B		          = 'shadow-b';
 SANE_NAME_HIGHLIGHT		          = 'highlight';
 SANE_NAME_HIGHLIGHT_R		          = 'highlight-r';
 SANE_NAME_HIGHLIGHT_G		          = 'highlight-g';
 SANE_NAME_HIGHLIGHT_B		          = 'highlight-b';
 SANE_NAME_HUE			          = 'hue';
 SANE_NAME_SATURATION		          = 'saturation';
 SANE_NAME_FILE			          = 'filename';
 SANE_NAME_HALFTONE_DIMENSION	          = 'halftone-size';
 SANE_NAME_HALFTONE_PATTERN	          = 'halftone-pattern';
 SANE_NAME_RESOLUTION_BIND	          = 'resolution-bind';
 SANE_NAME_NEGATIVE		          = 'negative';
 SANE_NAME_QUALITY_CAL		          = 'quality-cal';
 SANE_NAME_DOR			          = 'double-res';
 SANE_NAME_RGB_BIND		          = 'rgb-bind';
 SANE_NAME_THRESHOLD		          = 'threshold';
 SANE_NAME_ANALOG_GAMMA		          = 'analog-gamma';
 SANE_NAME_ANALOG_GAMMA_R	          = 'analog-gamma-r';
 SANE_NAME_ANALOG_GAMMA_G	          = 'analog-gamma-g';
 SANE_NAME_ANALOG_GAMMA_B	          = 'analog-gamma-b';
 SANE_NAME_ANALOG_GAMMA_BIND	          = 'analog-gamma-bind';
 SANE_NAME_WARMUP		          = 'warmup';
 SANE_NAME_CAL_EXPOS_TIME	          = 'cal-exposure-time';
 SANE_NAME_CAL_EXPOS_TIME_R	          = 'cal-exposure-time-r';
 SANE_NAME_CAL_EXPOS_TIME_G	          = 'cal-exposure-time-g';
 SANE_NAME_CAL_EXPOS_TIME_B	          = 'cal-exposure-time-b';
 SANE_NAME_SCAN_EXPOS_TIME	          = 'scan-exposure-time';
 SANE_NAME_SCAN_EXPOS_TIME_R	          = 'scan-exposure-time-r';
 SANE_NAME_SCAN_EXPOS_TIME_G	          = 'scan-exposure-time-g';
 SANE_NAME_SCAN_EXPOS_TIME_B	          = 'scan-exposure-time-b';
 SANE_NAME_SELECT_EXPOSURE_TIME	          = 'select-exposure-time';
 SANE_NAME_CAL_LAMP_DEN		          = 'cal-lamp-density';
 SANE_NAME_SCAN_LAMP_DEN		  = 'scan-lamp-density';
 SANE_NAME_SELECT_LAMP_DENSITY	          = 'select-lamp-density';


