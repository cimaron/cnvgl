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

cnvgl = {

	/**
	 * Current cnvGL context
	 *
	 * @var  cnvgl_context
	 */	
	currentContext : null,

	/**
	 * Create a new cnvgl context
	 *
	 * @var Object  driver  The graphics driver to attach to the context
	 */
	createContext : function(driver) {
		var ctx;
		ctx = new cnvgl.context(driver);
		return ctx;
	},

	/**
	 * Sets the current context
	 *
	 * @var cnvgl_context  context  The cnvgl context to make current
	 */
	setContext : function(context) {
		cnvgl.currentContext = context;
	},

	/**
	 * Gets the current context
	 */
	getCurrentContext : function() {
		return cnvgl.currentContext;
	},

	/**
	 * Sets an error in the specified context
	 *
	 * @var GLenum         error  Specifies the error code
	 * @var cnvgl_context  ctx    Specifies the context to set the error code
	 */
	throw_error : function(error, ctx) {
		ctx = ctx || cnvgl.getCurrentContext();
		if (error && ctx.errorValue == cnvgl.NO_ERROR) {
			ctx.errorValue = error;
		}
	}

};



@@include('./defines.js')

@@include('./objects/attrib_array_object.js')
@@include('./objects/buffer.js')
@@include('./objects/context.js')
@@include('./objects/context_shared.js')
@@include('./objects/framebuffer.js')
@@include('./objects/program.js')
@@include('./objects/shader.js')
@@include('./objects/renderbuffer.js')
@@include('./objects/texture.js')

@@include('./functions/blend.js')
@@include('./functions/bufferobj.js')
@@include('./functions/clear.js')
@@include('./functions/context.js')
@@include('./functions/depth.js')
@@include('./functions/draw.js')
@@include('./functions/enable.js')
@@include('./functions/fbobject.js')
@@include('./functions/get.js')
@@include('./functions/hint.js')
@@include('./functions/lines.js')
@@include('./functions/multisample.js')
@@include('./functions/pixelstore.js')
@@include('./functions/polygon.js')
@@include('./functions/scissor.js')
@@include('./functions/shaderapi.js')
@@include('./functions/stencil.js')
@@include('./functions/teximage.js')
@@include('./functions/texobj.js')
@@include('./functions/texparam.js')
@@include('./functions/texstate.js')
@@include('./functions/uniforms.js')
@@include('./functions/varray.js')
@@include('./functions/viewport.js')

@@include('./includes/memory.js')

