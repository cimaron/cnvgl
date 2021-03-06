/*
Copyright (c) 2011 Cimaron Shanahan

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


(function(cnvgl) {


	/**
	 * glBlendColor ? set the blend color
	 *
	 * @var GLclampf  red    specify the components of GL_BLEND_COLOR
	 * @var GLclampf  green
	 * @var GLclampf  blue
	 * @var GLclampf  alpha
	 *
	 * Notes: See http://www.opengl.org/sdk/docs/man/xhtml/glBlendColor.xml
	 */
	cnvgl.blendColor = function(red, green, blue, alpha) {
		var ctx, c;
		ctx = cnvgl.getCurrentContext();
		c = ctx.color.blendColor;
		c[0] = Math.round(255 * Math.max(Math.min(red, 1), 0));
		c[1] = Math.round(255 * Math.max(Math.min(green, 1), 0));
		c[2] = Math.round(255 * Math.max(Math.min(blue, 1), 0));
		c[3] = Math.round(255 * Math.max(Math.min(alpha, 1), 0));		

		ctx.driver.blendColor(ctx, red, green, blue, alpha);
	};

	/**
	 * glBlendEquationSeparate - specify the equation used for both the RGB blend equation and the Alpha blend equation
	 *
	 * @var   GLenum   mode
	 *
	 * Notes: See https://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendEquation.xml
	 */
	cnvgl.blendEquation = function(mode) {
		cnvgl.blendEquationSeparate(mode, mode);
	};

	/**
	 * glBlendEquationSeparate - set the RGB blend equation and the alpha blend equation separately
	 *
	 * @var   GLenum   mode
	 * @var   GLenum   mode
	 *
	 * Notes: See https://www.khronos.org/opengles/sdk/docs/man/xhtml/glBlendEquationSeparate.xml
	 */
	cnvgl.blendEquationSeparate = function(modeRGB, modeAlpha) {
		var ctx;

		ctx = cnvgl.getCurrentContext();
		
		//@todo: validate modes

		if (ctx.color.blendEquationRGB == modeRGB && ctx.color.blendEquationA == modeAlpha) {
			return;	
		}

		ctx.color.blendEquationRGB = modeRGB;
		ctx.color.blendEquationA = modeAlpha;

		ctx.driver.blendEquationSeparate(ctx, modeRGB, modeAlpha);
	};

	/**
	 * glBlendFunc ? specify pixel arithmetic
	 *
	 * @var GLenum  sfactor  Specifies how the red, green, blue, and alpha source blending factors are computed.
	 * @var GLenum  dfactor  Specifies how the red, green, blue, and alpha destination blending factors are computed.
	 *
	 * Notes: See http://www.opengl.org/sdk/docs/man/xhtml/glBlendFunc.xml
	 */
	cnvgl.blendFunc = function(sfactor, dfactor) {
		var ctx, i;

		ctx = cnvgl.getCurrentContext();
		
		//todo: add checks here
		
		ctx.color.blendSrcRGB = sfactor;
		ctx.color.blendSrcA = sfactor;
		ctx.color.blendDestRGB = dfactor;
		ctx.color.blendDestA = dfactor;
		
		ctx.driver.blendFunc(ctx, sfactor, dfactor);
	};

	/**
	 * glColorMask ? enable and disable writing of frame buffer color components
	 *
	 * @var GLboolean  red    Specify whether red, green, blue, and alpha can or cannot be written into the frame buffer.
	 * @var GLboolean  green
	 * @var GLboolean  blue
	 * @var GLboolean  alpha
	 *
	 * Notes: See http://www.opengl.org/sdk/docs/man/xhtml/glColorMask.xml
	 */
	cnvgl.colorMask = function(r, g, b, a) {
		var ctx;
		ctx = cnvgl.getCurrentContext();
	
		ctx.color.colorMask = [
			r ? 0xFF : 0,
			g ? 0xFF : 0,
			b ? 0xFF : 0,
			a ? 0xFF : 0
		];

		ctx.driver.colorMask(ctx, r, g, b, a);
	};
	
	
}(cnvgl));

