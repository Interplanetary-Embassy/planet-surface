(function () {
  'use strict';

  /*
   *
   * This is an adaptation of https://github.com/phoboslab/WebGLImageFilter
   * - Removed color filters and other parts not needed
   * - Partially updated syntax to ES6
   * - Updated code to run convolutions of arbitrary kernel size
   * - A few speed improvements
   * - Added Scale 3X filter
   *
   * -------------------------------------------------------------
   *
   * Adapted from WebGLImageFilter(MIT Licensed)
   * 2013, Dominic Szablewski - phoboslab.org
   *
   */

  const WebGLProgram = function (gl, vertexSource, fragmentSource) {
    const _collect = function (source, prefix, collection) {
      const r = new RegExp('\\b' + prefix + ' \\w+ (\\w+)', 'ig');
      source.replace(r, function (match, name) {
        collection[name] = 0;
        return match
      });
    };

    const _compile = function (gl, source, type) {
      const shader = gl.createShader(type);
      gl.shaderSource(shader, source);
      gl.compileShader(shader);

      if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.log(gl.getShaderInfoLog(shader));
        return null
      }
      return shader
    };

    this.uniform = {};
    this.attribute = {};

    const _vsh = _compile(gl, vertexSource, gl.VERTEX_SHADER);
    const _fsh = _compile(gl, fragmentSource, gl.FRAGMENT_SHADER);

    this.id = gl.createProgram();
    gl.attachShader(this.id, _vsh);
    gl.attachShader(this.id, _fsh);
    gl.linkProgram(this.id);

    if (!gl.getProgramParameter(this.id, gl.LINK_STATUS)) {
      console.log(gl.getProgramInfoLog(this.id));
    }

    gl.useProgram(this.id);

    // Collect attributes
    _collect(vertexSource, 'attribute', this.attribute);
    for (let a in this.attribute) {
      this.attribute[a] = gl.getAttribLocation(this.id, a);
    }

    // Collect uniforms
    _collect(vertexSource, 'uniform', this.uniform);
    _collect(fragmentSource, 'uniform', this.uniform);
    for (let u in this.uniform) {
      this.uniform[u] = gl.getUniformLocation(this.id, u);
    }
  };

  const WebGLImageFilter = function () {
    let _drawCount = 0;
    let _sourceTexture = null;
    let _lastInChain = false;
    let _currentFramebufferIndex = -1;
    let _tempFramebuffers = [null, null];
    let _filterChain = [];
    let _width = -1;
    let _height = -1;
    let _vertexBuffer = null;
    let _currentProgram = null;
    let _canvas = document.createElement('canvas');
    let gl = _canvas.getContext('webgl') || _canvas.getContext('experimental-webgl');

    if (!gl) {
      throw new Error("Couldn't get WebGL context")
    }

    this.addFilter = function (name, ...args) {
      const filter = _filter[name];
      _filterChain.push({func: filter, args});
    };

    this.reset = function () {
      _filterChain = [];
    };

    this.apply = function (image, imageOut) {
      _resize(image.width, image.height);
      _drawCount = 0;

      // Create the texture for the input image
      _sourceTexture = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, _sourceTexture);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);

      // No filters? Just draw
      if (_filterChain.length === 0) {
        _compileShader(SHADER.FRAGMENT_IDENTITY);
        _draw();
        return _canvas
      }

      for (let i = 0; i < _filterChain.length; i++) {
        _lastInChain = (i === _filterChain.length - 1);
        const f = _filterChain[i];

        f.func.apply(this, f.args || []);
      }

      gl.readPixels(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight, gl.RGBA, gl.UNSIGNED_BYTE, imageOut);
    };

    const _resize = function (width, height) {
      // Same width/height? Nothing to do here
      if (width === _width && height === _height) { return }

      _canvas.width = _width = width;
      _canvas.height = _height = height;

      // Create the context if we don't have it yet
      if (!_vertexBuffer) {
        // Create the vertex buffer for the two triangles [x, y, u, v] * 6
        const vertices = new Float32Array([
          -1, -1, 0, 1, 1, -1, 1, 1, -1, 1, 0, 0,
          -1, 1, 0, 0, 1, -1, 1, 1, 1, 1, 1, 0
        ]);
        _vertexBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, _vertexBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, vertices, gl.STATIC_DRAW);

        // Note sure if this is a good idea; at least it makes texture loading in Ejecta instant.
        gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, true);
      }

      gl.viewport(0, 0, _width, _height);

      // Delete old temp framebuffers
      _tempFramebuffers = [null, null];
    };

    const _getTempFramebuffer = function (index) {
      _tempFramebuffers[index] =
        _tempFramebuffers[index] ||
        _createFramebufferTexture(_width, _height);

      return _tempFramebuffers[index]
    };

    const _createFramebufferTexture = function (width, height) {
      const fbo = gl.createFramebuffer();
      gl.bindFramebuffer(gl.FRAMEBUFFER, fbo);

      const renderbuffer = gl.createRenderbuffer();
      gl.bindRenderbuffer(gl.RENDERBUFFER, renderbuffer);

      const texture = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);

      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);

      gl.bindTexture(gl.TEXTURE_2D, null);
      gl.bindFramebuffer(gl.FRAMEBUFFER, null);

      return {fbo: fbo, texture: texture}
    };

    const _draw = function (flags) {
      let source = null;
      let target = null;
      let flipY = false;

      // Set up the source
      if (_drawCount === 0) {
        // First draw call - use the source texture
        source = _sourceTexture;
      } else {
        // All following draw calls use the temp buffer last drawn to
        source = _getTempFramebuffer(_currentFramebufferIndex).texture;
      }
      _drawCount++;

                // Set up the target
      if (_lastInChain && !(flags & DRAW.INTERMEDIATE)) {
                        // Last filter in our chain - draw directly to the WebGL Canvas. We may
                        // also have to flip the image vertically now
        target = null;
        flipY = _drawCount % 2 === 1;
      } else {
                        // Intermediate draw call - get a temp buffer to draw to
        _currentFramebufferIndex = (_currentFramebufferIndex + 1) % 2;
        target = _getTempFramebuffer(_currentFramebufferIndex).fbo;
      }

                // Bind the source and target and draw the two triangles
      gl.bindTexture(gl.TEXTURE_2D, source);
      gl.bindFramebuffer(gl.FRAMEBUFFER, target);

      gl.uniform1f(_currentProgram.uniform.flipY, flipY ? -1 : 1);       // not working => (flipY ? -1 : 1)
      gl.drawArrays(gl.TRIANGLES, 0, 6);
    };

    const _compileShader = function (fragmentSource) {
      // if (fragmentSource.__program) {
      //   _currentProgram = fragmentSource
      //   gl.useProgram(_currentProgram.id)
      //   return _currentProgram
      // }

      // Compile shaders
      _currentProgram = new WebGLProgram(gl, SHADER.VERTEX_IDENTITY, fragmentSource);

      const floatSize = Float32Array.BYTES_PER_ELEMENT;
      const vertSize = 4 * floatSize;
      gl.enableVertexAttribArray(_currentProgram.attribute.pos);
      gl.vertexAttribPointer(_currentProgram.attribute.pos, 2, gl.FLOAT, false, vertSize, 0 * floatSize);
      gl.enableVertexAttribArray(_currentProgram.attribute.uv);
      gl.vertexAttribPointer(_currentProgram.attribute.uv, 2, gl.FLOAT, false, vertSize, 2 * floatSize);

      // fragmentSource.__program = _currentProgram
      return _currentProgram
    };

    const DRAW = { INTERMEDIATE: 1 };

    const SHADER = {};
    SHADER.VERTEX_IDENTITY = [
      'precision highp float;',
      'attribute vec2 pos;',
      'attribute vec2 uv;',
      'varying vec2 vUv;',
      'uniform float flipY;',

      'void main(void) {',
      'vUv = uv;',
      'gl_Position = vec4(pos.x, pos.y*flipY, 0.0, 1.);',
      '}'
    ].join('\n');

    SHADER.FRAGMENT_IDENTITY = [
      'precision highp float;',
      'varying vec2 vUv;',
      'uniform sampler2D texture;',

      'void main(void) {',
      'gl_FragColor = texture2D(texture, vUv);',
      '}'
    ].join('\n');

    const _filter = {};

    // ----------------------------------------------------------------------------
    // Convolution Filter

    _filter.convolution = function (matrix, inverseColors = false) {
      const pixelSizeX = 1 / _width;
      const pixelSizeY = 1 / _height;
      const shader = generateConvolutionShader(matrix, inverseColors);
      const program = _compileShader(shader);
      gl.uniform2f(program.uniform.px, pixelSizeX, pixelSizeY);
      _draw();
    };

    // Generate(and cache) a shader that perform the convolution of an image with a matrix of a given size
    // Set inverseColors to true to treat black as the maximum value(dilation of black is erosion of white)
    const convolutionShaderCache = new Map();
    const generateConvolutionShader = function (matrix, inverseColors = false) {
      const shaderKey = matrix.toString() + inverseColors.toString();
      if (!convolutionShaderCache.has(shaderKey)) {
        const matrixSize = matrix.length;
        if (matrixSize < 0 || matrixSize > 1000 || Math.sqrt(matrixSize) % 1 !== 0) throw new Error('invalid matrixSize')
        let convSize = Math.sqrt(matrixSize);
        if (convSize % 2 !== 1) throw new Error('invalid convSize')
        let halfSize = Math.floor(convSize / 2);
        let idx = 0;
        let inverseColorsStr = inverseColors ? '1.0 - ' : '';
        let fragColor = [];
        let shader = [
          'precision highp float;',
          'varying vec2 vUv;',
          'uniform sampler2D texture;',
          'uniform vec2 px;',
          'void main(void) {'];
        for (let i = 0; i < convSize; i++) {
          for (let j = 0; j < convSize; j++) {
            if (matrix[idx] !== 0 || (i === halfSize && j === halfSize)) {
              shader.push('vec4 c' + i + '_' + j + ' = ' + inverseColorsStr + 'texture2D(texture, vec2(vUv.x + ' + (j - halfSize).toFixed(1) + '*px.x, vUv.y + ' + (i - halfSize).toFixed(1) + '*px.y));');
              if (matrix[idx] === 1 || matrix[idx] === 1.0) {
                fragColor.push('c' + i + '_' + j);
              } else if (matrix[idx] % 1 === 0) {  // Force float notation even for rounded numbers
                fragColor.push('c' + i + '_' + j + ' * ' + matrix[idx].toFixed(1));
              } else {
                fragColor.push('c' + i + '_' + j + ' * ' + matrix[idx]);
              }
            }
            idx++;
          }
        }
        shader.push('gl_FragColor = ' + inverseColorsStr + '(' + fragColor.join(' + ') + ');');
        shader.push('gl_FragColor.a = ' + inverseColorsStr + 'c' + halfSize + '_' + halfSize + '.a;');
        shader.push('}');
        convolutionShaderCache.set(shaderKey, shader.join('\n'));
      }
      return convolutionShaderCache.get(shaderKey)
    };

    _filter.dilation = function () {
      _filter.convolution.call(this, [
        0, 1, 1, 1, 0,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        0, 1, 1, 1, 0
      ], false);
    };

    _filter.erosion = function () {
      _filter.convolution.call(this, [
        0, 1, 1, 1, 0,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        0, 1, 1, 1, 0
      ], true);
    };

    _filter.dilation3 = function () {
      _filter.convolution.call(this, [
        1, 1, 1,
        1, 1, 1,
        1, 1, 1
      ], false);
    };

    _filter.dilhor = function () {
      _filter.convolution.call(this, [
        0, 0, 0,
        1, 1, 1,
        0, 0, 0
      ], true);
    };

    _filter.surface = function () {
      _filter.convolution.call(this, [
        0, -255, 0,
        0, 1, 0,
        0, 0, 0
      ], false);
    };

    _filter.threshold = function () {
      _filter.convolution.call(this, [
        0, 0, 0,
        0, 255, 0,
        0, 0, 0
      ], false);
    };

    _filter.thresholdblack = function () {
      _filter.convolution.call(this, [
        0, 0, 0,
        0, 255, 0,
        0, 0, 0
      ], true);
    };

    _filter.avg3 = function () {
      _filter.convolution.call(this, [
        0.111111111111, 0.111111111111, 0.111111111111,
        0.111111111111, 0.111111111111, 0.111111111111,
        0.111111111111, 0.111111111111, 0.111111111111
      ]);
    };

    _filter.gaussian3 = function () {
      _filter.convolution.call(this, [
        0.077847, 0.123317, 0.077847,
        0.123317, 0.195346, 0.123317,
        0.077847, 0.123317, 0.077847
      ]);
    };

    _filter.gaussian5 = function () {
      _filter.convolution.call(this, [
        0.003765, 0.015019, 0.023792, 0.015019, 0.003765,
        0.015019, 0.059912, 0.094907, 0.059912, 0.015019,
        0.023792, 0.094907, 0.150342, 0.094907, 0.023792,
        0.015019, 0.059912, 0.094907, 0.059912, 0.015019,
        0.003765, 0.015019, 0.023792, 0.015019, 0.003765
      ]);
    };

    // ----------------------------------------------------------------------------
    // Line erosion Filter
    //

    _filter.surfaceErosion = function () {
      const pixelSizeX = 1 / _width;
      const pixelSizeY = 1 / _height;
      const program = _compileShader(_filter.surfaceErosion.SHADER);
      gl.uniform2f(program.uniform.px, pixelSizeX, pixelSizeY);
      _draw();
    };

    _filter.surfaceErosion.SHADER = [
      'precision highp float;',
      'varying vec2 vUv;',
      'uniform sampler2D texture;',
      'uniform vec2 px;',

      'void main(void) {',
      'vec4 A = texture2D(texture, vUv - px);', // top left
      'vec4 B = texture2D(texture, vec2(vUv.x - px.x, vUv.y) );', // mid left
      'vec4 C = texture2D(texture, vec2(vUv.x - px.x, vUv.y + px.y) );', // bottom left

      'vec4 D = texture2D(texture, vec2(vUv.x + px.x, vUv.y - px.y));', // top right
      'vec4 E = texture2D(texture, vec2(vUv.x + px.x, vUv.y) );', // mid right
      'vec4 F = texture2D(texture, vUv + px );', // bottom right

      'if ((A.r == 0.0 && B.r == 0.0 && C.r == 0.0) || (D.r == 0.0 && E.r == 0.0 && F.r == 0.0)) {',
      'gl_FragColor = vec4(0,0,0,0);',
      '} else {',
      'gl_FragColor = texture2D(texture, vUv);',
      '}',
      '}'
    ].join('\n');
  };

  // Perform a convolution on src image data using WebGL shaders
  // filters => an array of convolution filters to apply consecutively
  const imageFilter = new WebGLImageFilter();
  let uint8Buffer = null;
  function convolution (filters, src, dest) {
    if (!(src instanceof ImageData)) {
      throw new Error('Invalide src buffer type!!!' + arguments.toString())
    }
    imageFilter.reset();
    filters.forEach(f => imageFilter.addFilter(f));
    if (dest instanceof Uint8Array) {
      imageFilter.apply(src, dest);
    } else if (dest instanceof ImageData) {
      const bufferSize = dest.width * dest.height * 4;
      if (uint8Buffer == null || uint8Buffer.length !== bufferSize) {
        uint8Buffer = new Uint8Array(bufferSize);
      }
      imageFilter.apply(src, uint8Buffer);
      dest.data.set(uint8Buffer);
    } else {
      throw new Error('Invalide dest buffer type')
    }
  }

  /*
   *
   * This is a slightly modified copy of https://github.com/phoboslab/js-hqx
   * I only kept hq2x, see original github repo for hq3x and hq4x.
   * Added blackToAlpha option to switch channels before returning
   *
   * -------------------------------------------------------------
   *
   * Copyright (C) 2003 Maxim Stepin ( maxst@hiend3d.com )
   *
   * Copyright (C) 2010 Cameron Zemek ( grom@zeminvaders.net )
   *
   * Copyright (C) 2010 Dominic Szablewski ( mail@phoboslab.org )
   *
   * This program is free software; you can redistribute it and/or
   * modify it under the terms of the GNU Lesser General Public
   * License as published by the Free Software Foundation; either
   * version 2.1 of the License, or (at your option) any later version.
   *
   * This program is distributed in the hope that it will be useful,
   * but WITHOUT ANY WARRANTY; without even the implied warranty of
   * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   * Lesser General Public License for more details.
   *
   * You should have received a copy of the GNU Lesser General Public
   * License along with this program; if not, write to the Free Software
   * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
   */
  let _src = null;
  let _dest = null;
  const _MASK_2 = 0x00FF00;
  const _MASK_13 = 0xFF00FF;
  const _Ymask = 0x00FF0000;
  const _Umask = 0x0000FF00;
  const _Vmask = 0x000000FF;
  const _trY = 0x00300000;
  const _trU = 0x00000700;
  const _trV = 0x00000006;

  const _Math = window.Math; // global to local. SHALL NOT cache abs directly (http://jsperf.com/math-vs-global/2)

  const _RGBtoYUV = c => {
    const r = (c & 0xFF0000) >> 16;
    const g = (c & 0x00FF00) >> 8;
    const b = c & 0x0000FF;
    return (( /*y=*/ (0.299 * r + 0.587 * g + 0.114 * b | 0)) << 16) +
      (( /*u=*/ ((-0.169 * r - 0.331 * g + 0.5 * b) + 128 | 0)) << 8) +
      ( /*v=*/ ((0.5 * r - 0.419 * g - 0.081 * b) + 128 | 0));
  };

  const _Diff = (w1, w2) => {
    // Mask against RGB_MASK to discard the alpha channel
    const YUV1 = _RGBtoYUV(w1);
    const YUV2 = _RGBtoYUV(w2);
    return ((_Math.abs((YUV1 & _Ymask) - (YUV2 & _Ymask)) > _trY) ||
      (_Math.abs((YUV1 & _Umask) - (YUV2 & _Umask)) > _trU) ||
      (_Math.abs((YUV1 & _Vmask) - (YUV2 & _Vmask)) > _trV));
  };

  /* Interpolate functions */

  const _Interp1 = (pc, c1, c2) => {
    //*pc = (c1*3+c2) >> 2;
    if (c1 === c2) {
      _dest[pc] = c1;
      return;
    }
    _dest[pc] = ((((c1 & _MASK_2) * 3 + (c2 & _MASK_2)) >> 2) & _MASK_2) +
      ((((c1 & _MASK_13) * 3 + (c2 & _MASK_13)) >> 2) & _MASK_13);

    _dest[pc] |= (c1 & 0xFF000000);
  };

  const _Interp2 = (pc, c1, c2, c3) => {
    //*pc = (c1*2+c2+c3) >> 2;
    _dest[pc] = (((((c1 & _MASK_2) << 1) + (c2 & _MASK_2) + (c3 & _MASK_2)) >> 2) & _MASK_2) +
      (((((c1 & _MASK_13) << 1) + (c2 & _MASK_13) + (c3 & _MASK_13)) >> 2) & _MASK_13);

    _dest[pc] |= (c1 & 0xFF000000);
  };

  const _Interp6 = (pc, c1, c2, c3) => {
    //*pc = (c1*5+c2*2+c3)/8;
    _dest[pc] = ((((c1 & _MASK_2) * 5 + ((c2 & _MASK_2) << 1) + (c3 & _MASK_2)) >> 3) & _MASK_2) +
      ((((c1 & _MASK_13) * 5 + ((c2 & _MASK_13) << 1) + (c3 & _MASK_13)) >> 3) & _MASK_13);

    _dest[pc] |= (c1 & 0xFF000000);
  };

  const _Interp7 = (pc, c1, c2, c3) => {
    //*pc = (c1*6+c2+c3)/8;
    _dest[pc] = ((((c1 & _MASK_2) * 6 + (c2 & _MASK_2) + (c3 & _MASK_2)) >> 3) & _MASK_2) +
      ((((c1 & _MASK_13) * 6 + (c2 & _MASK_13) + (c3 & _MASK_13)) >> 3) & _MASK_13);

    _dest[pc] |= (c1 & 0xFF000000);
  };

  const _Interp9 = (pc, c1, c2, c3) => {
    //*pc = (c1*2+(c2+c3)*3)/8;
    _dest[pc] = (((((c1 & _MASK_2) << 1) + (c2 & _MASK_2) * 3 + (c3 & _MASK_2) * 3) >> 3) & _MASK_2) +
      (((((c1 & _MASK_13) << 1) + (c2 & _MASK_13) * 3 + (c3 & _MASK_13) * 3) >> 3) & _MASK_13);

    _dest[pc] |= (c1 & 0xFF000000);
  };


  const getVendorAttribute = (el, attr) => {
    const uc = attr.charAt(0).toUpperCase() + attr.substr(1);
    return el[attr] || el[`ms${uc}`] || el[`moz${uc}`] || el[`webkit${uc}`] || el[`o${uc}`];
  };


  // This function normalizes getImageData to extract the real, actual
  // pixels from an image. The naive method recently failed on retina
  // devices with a backgingStoreRatio != 1
  const getImagePixels = (image, x, y, width, height) => {
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');

    const ratio = getVendorAttribute(ctx, 'backingStorePixelRatio') || 1;
    ctx.getImageDataHD = getVendorAttribute(ctx, 'getImageDataHD');

    const realWidth = image.width / ratio;
    const realHeight = image.height / ratio;

    canvas.width = Math.ceil(realWidth);
    canvas.height = Math.ceil(realHeight);

    ctx.drawImage(image, 0, 0, realWidth, realHeight);

    return (ratio === 1) ?
      ctx.getImageData(x, y, width, height) :
      ctx.getImageDataHD(x, y, width, height);
  };

  // We can only scale with a factor of 2, see https://github.com/phoboslab/js-hqx for methods to scale 3 and 4
  function hqx (img, blackToAlpha) {
    const scale = 2;
    let orig;
    let origCtx;
    let scaled;
    let origPixels;
    if (img instanceof HTMLCanvasElement) {
      orig = img;
      origCtx = orig.getContext('2d');
      scaled = orig;
      origPixels = origCtx.getImageData(0, 0, orig.width, orig.height).data;
    } else {
      origPixels = getImagePixels(img, 0, 0, img.width, img.height).data;
      scaled = document.createElement('canvas');
    }


    // pack RGBA colors into integers
    const count = img.width * img.height;
    let src = _src = new Array(count);
    let dest = _dest = new Array(count * scale * scale);
    let index;
    for (let i = 0; i < count; i++) {
      src[i] = (origPixels[(index = i << 2) + 3] << 24) +
        (origPixels[index + 2] << 16) +
        (origPixels[index + 1] << 8) +
        origPixels[index];
    }

    // This is where the magic happens
    hq2x(img.width, img.height);

    scaled.width = img.width * scale;
    scaled.height = img.height * scale;

    const scaledCtx = scaled.getContext('2d');
    const scaledPixels = scaledCtx.getImageData(0, 0, scaled.width, scaled.height);
    const scaledPixelsData = scaledPixels.data;

    // unpack integers to RGBA
    let c;

    let a;
    const destLength = dest.length;
    if (blackToAlpha) {
      for (let j = 0; j < destLength; j++) {
        a = ((c = dest[j]) & 0xFF000000) >> 24;
        scaledPixelsData[(index = j << 2) + 3] = c & 0x000000FF; // Expect black/red image, set alpha to red value
        scaledPixelsData[index + 2] = 0;
        scaledPixelsData[index + 1] = 0;
        scaledPixelsData[index] = 255;
      }
    } else {
      for (let j = 0; j < destLength; j++) {
        a = ((c = dest[j]) & 0xFF000000) >> 24;
        scaledPixelsData[(index = j << 2) + 3] = a < 0 ? a + 256 : 0; // signed/unsigned :/
        scaledPixelsData[index + 2] = (c & 0x00FF0000) >> 16;
        scaledPixelsData[index + 1] = (c & 0x0000FF00) >> 8;
        scaledPixelsData[index] = c & 0x000000FF;
      }
    }
    _src = src = null;
    _dest = dest = null;
    scaledCtx.putImageData(scaledPixels, 0, 0);
    return scaled;
  }

  //------------------------------------------------------------------------------
  //------------------------------------------------------------------------------
  //------------------------------------------------------------------------------
  // hq 2x

  var hq2x = (width, height) => {
    let i;
    let j;
    let k;
    let prevline;
    let nextline;
    const w = [];

    const //dpL = width * 2, optimized
      dpL = width << 1;

    let dp = 0;
    let sp = 0;

    // internal to local optimization
    const Diff = _Diff;

    const Math = _Math;
    const RGBtoYUV = _RGBtoYUV;
    const Interp1 = _Interp1;
    const Interp2 = _Interp2;
    const Interp6 = _Interp6;
    const Interp7 = _Interp7;
    const Interp9 = _Interp9;
    const src = _src;
    const dest = _dest;
    const Ymask = _Ymask;
    const Umask = _Umask;
    const Vmask = _Vmask;
    const trY = _trY;
    const trU = _trU;
    const trV = _trV;
    let YUV1;
    let YUV2;


    //   +----+----+----+
    //   |    |    |    |
    //   | w1 | w2 | w3 |
    //   +----+----+----+
    //   |    |    |    |
    //   | w4 | w5 | w6 |
    //   +----+----+----+
    //   |    |    |    |
    //   | w7 | w8 | w9 |
    //   +----+----+----+

    for (j = 0; j < height; j++) {
      prevline = j > 0 ? -width : 0;
      nextline = j < height - 1 ? width : 0;

      for (i = 0; i < width; i++) {
        w[2] = src[sp + prevline];
        w[5] = src[sp];
        w[8] = src[sp + nextline];

        if (i > 0) {
          w[1] = src[sp + prevline - 1];
          w[4] = src[sp - 1];
          w[7] = src[sp + nextline - 1];
        } else {
          w[1] = w[2];
          w[4] = w[5];
          w[7] = w[8];
        }

        if (i < width - 1) {
          w[3] = src[sp + prevline + 1];
          w[6] = src[sp + 1];
          w[9] = src[sp + nextline + 1];
        } else {
          w[3] = w[2];
          w[6] = w[5];
          w[9] = w[8];
        }

        let pattern = 0;
        let flag = 1;

        YUV1 = RGBtoYUV(w[5]);

        //for (k=1; k<=9; k++) optimized
        for (k = 1; k < 10; k++) // k<=9
        {
          if (k === 5) continue;

          if (w[k] !== w[5]) {
            YUV2 = RGBtoYUV(w[k]);
            if ((Math.abs((YUV1 & Ymask) - (YUV2 & Ymask)) > trY) ||
              (Math.abs((YUV1 & Umask) - (YUV2 & Umask)) > trU) ||
              (Math.abs((YUV1 & Vmask) - (YUV2 & Vmask)) > trV))
              pattern |= flag;
          }
          flag <<= 1;
        }

        switch (pattern) {
          case 0:
          case 1:
          case 4:
          case 32:
          case 128:
          case 5:
          case 132:
          case 160:
          case 33:
          case 129:
          case 36:
          case 133:
          case 164:
          case 161:
          case 37:
          case 165:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 2:
          case 34:
          case 130:
          case 162:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 16:
          case 17:
          case 48:
          case 49:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 64:
          case 65:
          case 68:
          case 69:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 8:
          case 12:
          case 136:
          case 140:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 3:
          case 35:
          case 131:
          case 163:
            {
              Interp1(dp, w[5], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 6:
          case 38:
          case 134:
          case 166:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 20:
          case 21:
          case 52:
          case 53:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 144:
          case 145:
          case 176:
          case 177:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 192:
          case 193:
          case 196:
          case 197:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 96:
          case 97:
          case 100:
          case 101:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 40:
          case 44:
          case 168:
          case 172:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 9:
          case 13:
          case 137:
          case 141:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 18:
          case 50:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 80:
          case 81:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 72:
          case 76:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 10:
          case 138:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 66:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 24:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 7:
          case 39:
          case 135:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 148:
          case 149:
          case 180:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 224:
          case 228:
          case 225:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 41:
          case 169:
          case 45:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 22:
          case 54:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 208:
          case 209:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 104:
          case 108:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 11:
          case 139:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 19:
          case 51:
            {
              if (Diff(w[2], w[6])) {
                Interp1(dp, w[5], w[4]);
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp6(dp, w[5], w[2], w[4]);
                Interp9(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 146:
          case 178:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
                Interp1(dp + dpL + 1, w[5], w[8]);
              } else {
                Interp9(dp + 1, w[5], w[2], w[6]);
                Interp6(dp + dpL + 1, w[5], w[6], w[8]);
              }
              Interp2(dp + dpL, w[5], w[8], w[4]);
              break;
            }
          case 84:
          case 85:
            {
              Interp2(dp, w[5], w[4], w[2]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + 1, w[5], w[2]);
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp6(dp + 1, w[5], w[6], w[2]);
                Interp9(dp + dpL + 1, w[5], w[6], w[8]);
              }
              Interp2(dp + dpL, w[5], w[7], w[4]);
              break;
            }
          case 112:
          case 113:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL, w[5], w[4]);
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp6(dp + dpL, w[5], w[8], w[4]);
                Interp9(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 200:
          case 204:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
                Interp1(dp + dpL + 1, w[5], w[6]);
              } else {
                Interp9(dp + dpL, w[5], w[8], w[4]);
                Interp6(dp + dpL + 1, w[5], w[8], w[6]);
              }
              break;
            }
          case 73:
          case 77:
            {
              if (Diff(w[8], w[4])) {
                Interp1(dp, w[5], w[2]);
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp6(dp, w[5], w[4], w[2]);
                Interp9(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 42:
          case 170:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
                Interp1(dp + dpL, w[5], w[8]);
              } else {
                Interp9(dp, w[5], w[4], w[2]);
                Interp6(dp + dpL, w[5], w[4], w[8]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 14:
          case 142:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
                Interp1(dp + 1, w[5], w[6]);
              } else {
                Interp9(dp, w[5], w[4], w[2]);
                Interp6(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 67:
            {
              Interp1(dp, w[5], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 70:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 28:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 152:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 194:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 98:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 56:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 25:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 26:
          case 31:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 82:
          case 214:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 88:
          case 248:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 74:
          case 107:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 27:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[3]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 86:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 216:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp1(dp + dpL, w[5], w[7]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 106:
            {
              Interp1(dp, w[5], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 30:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 210:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp1(dp + 1, w[5], w[3]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 120:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 75:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp1(dp + dpL, w[5], w[7]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 29:
            {
              Interp1(dp, w[5], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 198:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 184:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 99:
            {
              Interp1(dp, w[5], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 57:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 71:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 156:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 226:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 60:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 195:
            {
              Interp1(dp, w[5], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 102:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 153:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 58:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 83:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[4]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 92:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 202:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 78:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[6]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 154:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 114:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[4]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 89:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 90:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 55:
          case 23:
            {
              if (Diff(w[2], w[6])) {
                Interp1(dp, w[5], w[4]);
                dest[dp + 1] = w[5];
              } else {
                Interp6(dp, w[5], w[2], w[4]);
                Interp9(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 182:
          case 150:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
                Interp1(dp + dpL + 1, w[5], w[8]);
              } else {
                Interp9(dp + 1, w[5], w[2], w[6]);
                Interp6(dp + dpL + 1, w[5], w[6], w[8]);
              }
              Interp2(dp + dpL, w[5], w[8], w[4]);
              break;
            }
          case 213:
          case 212:
            {
              Interp2(dp, w[5], w[4], w[2]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + 1, w[5], w[2]);
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp6(dp + 1, w[5], w[6], w[2]);
                Interp9(dp + dpL + 1, w[5], w[6], w[8]);
              }
              Interp2(dp + dpL, w[5], w[7], w[4]);
              break;
            }
          case 241:
          case 240:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL, w[5], w[4]);
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp6(dp + dpL, w[5], w[8], w[4]);
                Interp9(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 236:
          case 232:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
                Interp1(dp + dpL + 1, w[5], w[6]);
              } else {
                Interp9(dp + dpL, w[5], w[8], w[4]);
                Interp6(dp + dpL + 1, w[5], w[8], w[6]);
              }
              break;
            }
          case 109:
          case 105:
            {
              if (Diff(w[8], w[4])) {
                Interp1(dp, w[5], w[2]);
                dest[dp + dpL] = w[5];
              } else {
                Interp6(dp, w[5], w[4], w[2]);
                Interp9(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 171:
          case 43:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
                Interp1(dp + dpL, w[5], w[8]);
              } else {
                Interp9(dp, w[5], w[4], w[2]);
                Interp6(dp + dpL, w[5], w[4], w[8]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 143:
          case 15:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
                Interp1(dp + 1, w[5], w[6]);
              } else {
                Interp9(dp, w[5], w[4], w[2]);
                Interp6(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 124:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 203:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp1(dp + dpL, w[5], w[7]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 62:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 211:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[3]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 118:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[4]);
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 217:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp1(dp + dpL, w[5], w[7]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 110:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 155:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[3]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 188:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 185:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 61:
            {
              Interp1(dp, w[5], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 157:
            {
              Interp1(dp, w[5], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 103:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 227:
            {
              Interp1(dp, w[5], w[4]);
              Interp2(dp + 1, w[5], w[3], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 230:
            {
              Interp2(dp, w[5], w[1], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 199:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp2(dp + dpL, w[5], w[7], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 220:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 158:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 234:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 242:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 59:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 121:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 87:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[7], w[4]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 79:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[6]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 122:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 94:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 218:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 91:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 229:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 167:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 173:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 181:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 186:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 115:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[4]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 93:
            {
              Interp1(dp, w[5], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 206:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[6]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 205:
          case 201:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              if (Diff(w[8], w[4])) {
                Interp1(dp + dpL, w[5], w[7]);
              } else {
                Interp7(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 174:
          case 46:
            {
              if (Diff(w[4], w[2])) {
                Interp1(dp, w[5], w[4]);
              } else {
                Interp7(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[6]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 179:
          case 147:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                Interp1(dp + 1, w[5], w[3]);
              } else {
                Interp7(dp + 1, w[5], w[2], w[6]);
              }
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 117:
          case 116:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp1(dp + dpL, w[5], w[4]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL + 1, w[5], w[9]);
              } else {
                Interp7(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 189:
            {
              Interp1(dp, w[5], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 231:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              Interp1(dp + dpL, w[5], w[4]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 126:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 219:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[3]);
              Interp1(dp + dpL, w[5], w[7]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 125:
            {
              if (Diff(w[8], w[4])) {
                Interp1(dp, w[5], w[2]);
                dest[dp + dpL] = w[5];
              } else {
                Interp6(dp, w[5], w[4], w[2]);
                Interp9(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + 1, w[5], w[2]);
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 221:
            {
              Interp1(dp, w[5], w[2]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + 1, w[5], w[2]);
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp6(dp + 1, w[5], w[6], w[2]);
                Interp9(dp + dpL + 1, w[5], w[6], w[8]);
              }
              Interp1(dp + dpL, w[5], w[7]);
              break;
            }
          case 207:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
                Interp1(dp + 1, w[5], w[6]);
              } else {
                Interp9(dp, w[5], w[4], w[2]);
                Interp6(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[7]);
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 238:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
                Interp1(dp + dpL + 1, w[5], w[6]);
              } else {
                Interp9(dp + dpL, w[5], w[8], w[4]);
                Interp6(dp + dpL + 1, w[5], w[8], w[6]);
              }
              break;
            }
          case 190:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
                Interp1(dp + dpL + 1, w[5], w[8]);
              } else {
                Interp9(dp + 1, w[5], w[2], w[6]);
                Interp6(dp + dpL + 1, w[5], w[6], w[8]);
              }
              Interp1(dp + dpL, w[5], w[8]);
              break;
            }
          case 187:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
                Interp1(dp + dpL, w[5], w[8]);
              } else {
                Interp9(dp, w[5], w[4], w[2]);
                Interp6(dp + dpL, w[5], w[4], w[8]);
              }
              Interp1(dp + 1, w[5], w[3]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 243:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[3]);
              if (Diff(w[6], w[8])) {
                Interp1(dp + dpL, w[5], w[4]);
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp6(dp + dpL, w[5], w[8], w[4]);
                Interp9(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 119:
            {
              if (Diff(w[2], w[6])) {
                Interp1(dp, w[5], w[4]);
                dest[dp + 1] = w[5];
              } else {
                Interp6(dp, w[5], w[2], w[4]);
                Interp9(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[4]);
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 237:
          case 233:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[2], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp1(dp + dpL, w[5], w[7]);
              }
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 175:
          case 47:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp1(dp, w[5], w[4]);
              }
              Interp1(dp + 1, w[5], w[6]);
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              break;
            }
          case 183:
          case 151:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp1(dp + 1, w[5], w[3]);
              }
              Interp2(dp + dpL, w[5], w[8], w[4]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 245:
          case 244:
            {
              Interp2(dp, w[5], w[4], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              Interp1(dp + dpL, w[5], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp1(dp + dpL + 1, w[5], w[9]);
              }
              break;
            }
          case 250:
            {
              Interp1(dp, w[5], w[4]);
              Interp1(dp + 1, w[5], w[3]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 123:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[3]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 95:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[7]);
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 222:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[7]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 252:
            {
              Interp2(dp, w[5], w[1], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp1(dp + dpL + 1, w[5], w[9]);
              }
              break;
            }
          case 249:
            {
              Interp1(dp, w[5], w[2]);
              Interp2(dp + 1, w[5], w[3], w[2]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp1(dp + dpL, w[5], w[7]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 235:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp2(dp + 1, w[5], w[3], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp1(dp + dpL, w[5], w[7]);
              }
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 111:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp1(dp, w[5], w[4]);
              }
              Interp1(dp + 1, w[5], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp2(dp + dpL + 1, w[5], w[9], w[6]);
              break;
            }
          case 63:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp1(dp, w[5], w[4]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[8]);
              Interp2(dp + dpL + 1, w[5], w[9], w[8]);
              break;
            }
          case 159:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp1(dp + 1, w[5], w[3]);
              }
              Interp2(dp + dpL, w[5], w[7], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 215:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp1(dp + 1, w[5], w[3]);
              }
              Interp2(dp + dpL, w[5], w[7], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 246:
            {
              Interp2(dp, w[5], w[1], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              Interp1(dp + dpL, w[5], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp1(dp + dpL + 1, w[5], w[9]);
              }
              break;
            }
          case 254:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp1(dp + dpL + 1, w[5], w[9]);
              }
              break;
            }
          case 253:
            {
              Interp1(dp, w[5], w[2]);
              Interp1(dp + 1, w[5], w[2]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp1(dp + dpL, w[5], w[7]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp1(dp + dpL + 1, w[5], w[9]);
              }
              break;
            }
          case 251:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              Interp1(dp + 1, w[5], w[3]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp1(dp + dpL, w[5], w[7]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 239:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp1(dp, w[5], w[4]);
              }
              Interp1(dp + 1, w[5], w[6]);
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp1(dp + dpL, w[5], w[7]);
              }
              Interp1(dp + dpL + 1, w[5], w[6]);
              break;
            }
          case 127:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp1(dp, w[5], w[4]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp2(dp + 1, w[5], w[2], w[6]);
              }
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp2(dp + dpL, w[5], w[8], w[4]);
              }
              Interp1(dp + dpL + 1, w[5], w[9]);
              break;
            }
          case 191:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp1(dp, w[5], w[4]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp1(dp + 1, w[5], w[3]);
              }
              Interp1(dp + dpL, w[5], w[8]);
              Interp1(dp + dpL + 1, w[5], w[8]);
              break;
            }
          case 223:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp2(dp, w[5], w[4], w[2]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp1(dp + 1, w[5], w[3]);
              }
              Interp1(dp + dpL, w[5], w[7]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp2(dp + dpL + 1, w[5], w[6], w[8]);
              }
              break;
            }
          case 247:
            {
              Interp1(dp, w[5], w[4]);
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp1(dp + 1, w[5], w[3]);
              }
              Interp1(dp + dpL, w[5], w[4]);
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp1(dp + dpL + 1, w[5], w[9]);
              }
              break;
            }
          case 255:
            {
              if (Diff(w[4], w[2])) {
                dest[dp] = w[5];
              } else {
                Interp1(dp, w[5], w[4]);
              }
              if (Diff(w[2], w[6])) {
                dest[dp + 1] = w[5];
              } else {
                Interp1(dp + 1, w[5], w[3]);
              }
              if (Diff(w[8], w[4])) {
                dest[dp + dpL] = w[5];
              } else {
                Interp1(dp + dpL, w[5], w[7]);
              }
              if (Diff(w[6], w[8])) {
                dest[dp + dpL + 1] = w[5];
              } else {
                Interp1(dp + dpL + 1, w[5], w[9]);
              }
              break;
            }
        }
        sp++;
        dp += 2;
      }
      dp += dpL;
    }
  };

  /*
   *
   * This is an adapted version from https://github.com/josephg/noisejs
   * I just updated the syntax & formatting.
   *
   * -------------------------------------------------------------
   *
   * A speed-improved perlin and simplex noise algorithms for 2D.
   *
   * Based on example code by Stefan Gustavson (stegu@itn.liu.se).
   * Optimisations by Peter Eastman (peastman@drizzle.stanford.edu).
   * Better rank ordering method by Stefan Gustavson in 2012.
   * Converted to Javascript by Joseph Gentle.
   *
   */

  class Grad {
    constructor (x, y, z) {
      this.x = x;
      this.y = y;
      this.z = z;
    }

    dot2 (x, y) {
      return (this.x * x) + (this.y * y)
    }

    dot3 (x, y, z) {
      return (this.x * x) + (this.y * y) + (this.z * z)
    }
  }

  const grad3 = [
    new Grad(1, 1, 0),
    new Grad(-1, 1, 0),
    new Grad(1, -1, 0),
    new Grad(-1, -1, 0),
    new Grad(1, 0, 1),
    new Grad(-1, 0, 1),
    new Grad(1, 0, -1),
    new Grad(-1, 0, -1),
    new Grad(0, 1, 1),
    new Grad(0, -1, 1),
    new Grad(0, 1, -1),
    new Grad(0, -1, -1)
  ];

  const p = [
    151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140,
    36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234,
    75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237,
    149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48,
    27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92,
    41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209,
    76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164,
    100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147,
    118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28,
    42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155,
    167, 43, 172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185,
    112, 104, 218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162,
    241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157,
    184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222,
    114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180
  ];

  // To remove the need for index wrapping, double the permutation table length
  const perm = new Array(512);
  const gradP = new Array(512);

  // This isn't a very good seeding function, but it works ok. It supports 2^16
  // different seed values. Write something better if you need more seeds.
  function seed (initialSeed) {
     /* eslint-disable no-bitwise */
    let s = initialSeed;

    if (s > 0 && s < 1) {
      // Scale the s out
      s *= 65536;
    }

    s = Math.floor(s);
    if (s < 256) {
      s |= s << 8;
    }

    for (let i = 0; i < 256; i++) {
      let v;
      if (i & 1) {
        v = p[i] ^ (s & 255);
      } else {
        v = p[i] ^ ((s >> 8) & 255);
      }

      perm[i] = perm[i + 256] = v;
      gradP[i] = gradP[i + 256] = grad3[v % 12];
    }

    /* eslint-enable no-bitwise */
  }

  // ##### Perlin noise stuff

  function fade (t) {
    return t * t * t * (t * (t * 6 - 15) + 10)
  }

  function lerp (a, b, t) {
    return ((1 - t) * a) + (t * b)
  }

  // 2D Perlin Noise
  function perlin2 (x, y) {
    // Find unit grid cell containing point
    let X = Math.floor(x);
    let Y = Math.floor(y);
    // Get relative xy coordinates of point within that cell
    x -= X;
    y -= Y;
    // Wrap the integer cells at 255 (smaller integer period can be introduced here)
    X &= 255;
    Y &= 255;

    // Calculate noise contributions from each of the four corners
    const n00 = gradP[X + perm[Y]].dot2(x, y);
    const n01 = gradP[X + perm[Y + 1]].dot2(x, y - 1);
    const n10 = gradP[X + 1 + perm[Y]].dot2(x - 1, y);
    const n11 = gradP[X + 1 + perm[Y + 1]].dot2(x - 1, y - 1);

    // Compute the fade curve value for x
    const u = fade(x);

    // Interpolate the four results
    return lerp(lerp(n00, n10, u), lerp(n01, n11, u), fade(y))
  }

  seed(0);

  // Generate deterministic pseudo random numbers from a seed in range [0,1)
  // Adapted from: https://gist.github.com/blixt/f17b47c62508be59987b
  class Random {
    constructor (seed) {
      if (seed < 0 || seed >= 1) {
        throw new Error('Invalid seed: ' + seed + ', must be between [0,1).')
      }
      this.seed = seed === 0
        ? 2147483646
        : Math.floor(seed * Math.pow(2, 53)) % 2147483647;
    }

    nextInt () {
      // Returns a pseudo-random integer value between 1 and 2^31 - 2
      this.seed = this.seed * 16807 % 2147483647;
      return this.seed
    }

    nextFloat () {
      // We know that result of next() will be 1 to 2147483646 (inclusive).
      return (this.nextInt() - 1) / 2147483646
    }

    nextIntBetween (min, max) {
      return Math.floor(this.nextFloat() * (max - min + 1)) + min
    }
  }

  // Convert hex color notation to RGB components
  // from https://stackoverflow.com/a/5624139/257272
  function hexToRgb (hex) {
    const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    return result ? {
      r: parseInt(result[1], 16),
      g: parseInt(result[2], 16),
      b: parseInt(result[3], 16)
    } : null
  }

  // Use canvas API to resize some ImageData
  function scaleImageData (imgData, destWidth, destHeight, isSmoothed) {
    const srcCanvas = document.createElement('canvas');
    srcCanvas.width = imgData.width;
    srcCanvas.height = imgData.height;
    srcCanvas.getContext('2d').putImageData(imgData, 0, 0);

    const dstCanvas = document.createElement('canvas');
    dstCanvas.width = destWidth;
    dstCanvas.height = destHeight;
    const dstCtx = dstCanvas.getContext('2d');

    dstCtx.imageSmoothingEnabled = isSmoothed;
    dstCtx.mozImageSmoothingEnabled = isSmoothed;
    dstCtx.webkitImageSmoothingEnabled = isSmoothed;
    dstCtx.msImageSmoothingEnabled = isSmoothed;

    dstCtx.drawImage(srcCanvas, 0, 0, imgData.width, imgData.height, 0, 0, destWidth, destHeight);
    return dstCtx.getImageData(0, 0, destWidth, destHeight)
  }

  // Draw src image to dest canvas, optionaly resizing by scaleFactor
  function drawStepToCanvas (src, dest, scaleFactor) {
    scaleFactor = scaleFactor || 1;
    if (typeof dest === 'string') {
      dest = document.getElementById(dest);
    }
    dest.classList.remove('loading');
    dest.width = Math.floor(src.width * scaleFactor);
    dest.height = Math.floor(src.height * scaleFactor);
    if (src instanceof HTMLCanvasElement) {
      dest.getContext('2d').drawImage(src, 0, 0, dest.width, dest.height);
    } else if (src instanceof ImageData) {
      if (scaleFactor !== 1) {
        src = scaleImageData(src, dest.width, dest.height, false);
      }
      dest.getContext('2d').putImageData(src, 0, 0);
    } else {
      throw new Error('src should be a HTMLCanvasElement or ImageData object')
    }
  }

  // Load an html img into an ImageData array(optionnaly scale it to given size)
  const imageDataCanvas = document.createElement('canvas');
  const context = imageDataCanvas.getContext('2d');
  function getHtmlImageData (img, scaleToSize) {
    if (!scaleToSize) {
      scaleToSize = {
        width: img.width,
        height: img.height
      };
    }
    imageDataCanvas.width = scaleToSize.width;
    imageDataCanvas.height = scaleToSize.height;
    context.imageSmoothingEnabled = false;
    context.drawImage(img, 0, 0, img.width, img.height, 0, 0, scaleToSize.width, scaleToSize.height);
    return context.getImageData(0, 0, scaleToSize.width, scaleToSize.height)
  }

  // Load an image from its URL
  const imageCache = new Map();
  function loadImage (src) {
    const imageId = `${src}`;
    return imageCache.has(imageId)
      ? Promise.resolve(imageCache.get(imageId))
      : new Promise((resolve) => {
        const img = new Image();
        img.src = src;
        img.onload = () => {
          imageCache.set(imageId, img);
          resolve(img);
        };
      })
  }

  // A simple class to time our methods
  class Timer {
    constructor () {
      this.entries = {};
    }

    // Start an iteration of the timer with the given name
    start (name) {
      if (!(name in this.entries)) {
        this.entries[name] = { time: null, last: 0, avg: 0, nb: 0 };
      }
      this.entries[name].time = new Date();
    }

    // Stop an iteration of the timer with the given name
    stop (name) {
      if (!(name in this.entries)) {
        throw new Error('The timer ' + name + ' hasn\'t been started')
      }
      this.entries[name].last = (new Date()).getTime() - this.entries[name].time.getTime();
      this.entries[name].avg += this.entries[name].last;
      this.entries[name].time = null;
      this.entries[name].nb++;
    }

    // Write results to html elements with corresponding ids for the demo
    toHtmlElts () {
      const timerEltPrefix = 'timer-';
      for (let i in this.entries) {
        const elt = document.getElementById(timerEltPrefix + i);
        if (elt) {
          elt.innerHTML = '(' + this.entries[i].last + 'ms)';
        }
      }
      const elt = document.getElementById(timerEltPrefix + 'render-terrain');
      if (elt && 'draw-terrain' in this.entries && 'position-generator' in this.entries) {
        elt.innerHTML = '(' + (this.entries['draw-terrain'].last + this.entries['position-generator'].last) + 'ms)';
      }
    }

    // Write all timers info to a string that can be displayed in a textarea
    toString () {
      let str = '--Timers--\n';
      for (let i in this.entries) {
        str += i + ': avg=' + this.entries[i].avg / this.entries[i].nb + ' (' + this.entries[i].nb + ' occurences)\n';
      }
      return str
    }
  }

  const timer = new Timer();

  const DEFAULT_OPTIONS = {
    debug: false,
    blackToAlpha: true,
    width: 1024,
    height: 612,
    noiseResolution: 35,
    noiseResolutionBlack: 18,
    noiseThreshold: 20.0,
    terrainTypeImg: null
  };

  // Generate random terrain shape corresponding to a terrain type
  class TerrainGenerator {

    // Factory method to create a TerrainGenerator from an img url instead of an img object
    static async fromImgUrl (opts) {
      const imgOpts = Object.assign({}, opts, { terrainTypeImg: await loadImage(opts.terrainTypeImg) });
      return new TerrainGenerator(imgOpts)
    }

    constructor (opts) {
      // Check options values
      this.options = Object.assign({}, DEFAULT_OPTIONS, opts);
      if (this.options.width % 2 !== 0 || this.options.height % 2) {
        throw new Error('Terrain width and height should be even')
      }
      if (!this.options.terrainTypeImg) {
        throw new Error('Required terrainTypeImg option must be an HTMLImageElement(or a CanvasImageSource)')
      }

      // We will work on images 4 times smaller than the final resolution
      // then magnify it at the last step
      this.w = this.options.width / 2;
      this.h = this.options.height / 2;
      this.terr = new ImageData(this.w, this.h);
      this.terrainType = getHtmlImageData(this.options.terrainTypeImg, { width: this.w, height: this.h });
      this.terrainTypePoints = this.loadTerrainTypePoints();
    }

    // Compute and return the following:
    // fgPoints : An array of points (x,y) at the contour of the core of the terrain
    // bgPoints: An array of points (x,y) at the border representing the background
    loadTerrainTypePoints (threshold = 100) {
      let points = { fg: [], bg: [] };
      const td = this.terrainType.data;
      for (let x = 0; x < this.w; x++) {
        for (let y = 0; y < this.h; y++) {
          if (td[(x + y * this.w) * 4] < threshold && (
              td[(x - 1 + y * this.w) * 4] > threshold ||
              td[(x + 1 + y * this.w) * 4] > threshold ||
              td[(x + (y + 1) * this.w) * 4] > threshold ||
              td[(x + (y - 1) * this.w) * 4] > threshold)) {
            points.fg.push([x, y]);
          } else if (td[(x + y * this.w) * 4] < threshold && td[2 + (x + y * this.w) * 4] < threshold && (
              x === 0 ||
              y === 0 ||
              x === this.w - 1 ||
              y === this.h - 1)) {
            points.bg.push([x, y]);
          }
        }
      }
      return points
    }

    // Apply Perlin noise to a terrain image
    // The noise resolution will be different for blue zones vs. the rest
    perlinNoise (seed$$1) {
      const perlin = perlin2; // Something with webpack makes it faster than using the reference directly !!!
      const d = this.terr.data;
      seed(seed$$1);
      for (let x = 0; x < this.w; x++) {
        for (let y = 0; y < this.h; y++) {
          let pix = (x + y * this.w) * 4;

          let value = Math.abs(perlin(x / this.options.noiseResolution, y / this.options.noiseResolution));
          value = Math.max(0, (25 - value * 256) * 8);

          // A second value with different noise is calculated for black terrain pixels
          if (d[pix] === 0 && d[2 + pix] === 0) {
            let value2 = Math.abs(perlin((this.w - x - 1) / this.options.noiseResolutionBlack, y / this.options.noiseResolutionBlack));
            value2 = Math.max(0, (25 - value2 * 256) * 8);
            value = (value + value2) / 2.0;
          }

          // Do not touch red pixels
          if (d[pix] < 200) {
            d[pix] = d[1 + pix] = d[2 + pix] = 0;
            if (value > this.options.noiseThreshold) {
              d[pix] = d[1 + pix] = 255;
            }
          }
        }
      }
    }

    removePerlin () {
      const d = this.terr.data;
      for (let x = 0; x < this.w; x++) {
        for (let y = 0; y < this.h; y++) {
          let pix = (x + y * this.w) * 4;
          if (d[pix] === 255 && d[pix + 1] === 255) {
            d[pix] = (d[pix + 2] === 255 ? 255 : 0);
            d[pix + 1] = 0;
          }
        }
      }
    }

    paintBackground () {
      // Floodfill from background points at the border
      this.floodFill(this.terrainTypePoints.bg.slice(), {
        channel: 1
      });
    }

    removeHoles () {
      // Replace remaining black area with terrain(ie. red) and background back to black
      const d = this.terr.data;
      for (let x = 0; x < this.w; x++) {
        for (let y = 0; y < this.h; y++) {
          const pix = (x + y * this.w) * 4;
          if (d[pix] === 255 && d[1 + pix] === 255) {
            d[pix] = d[1 + pix] = 0;
          } else {
            d[pix] = 255;
          }
        }
      }
    }

    // Fast scanline floodfill algo adapted from http://lodev.org/cgtutor/floodfill.html#Recursive_Scanline_Floodfill_Algorithm
    // stack is an array of points [x,y] to floodfill
    // channel is color of floodfill 0 => R, 1 => Y
    floodFill (stack, {channel = 0, threshold = 255}) {
      let pt;
      let x;
      let y;
      let x1;
      let spanAbove;
      let spanBelow;
      const d = this.terr.data;
      const w = this.w;
      const h = this.h;

      while (stack.length > 0) {
        pt = stack.pop();
        x = pt[0];
        y = pt[1];
        x1 = x;
        while (x1 >= 0 && d[(x1 + y * w) * 4] < threshold) { x1--; }
        x1++;
        spanAbove = 0;
        spanBelow = 0;
        while (x1 < w && d[(x1 + y * w) * 4] < threshold) {
          d[(x1 + y * w) * 4] = 255;
          d[channel + (x1 + y * w) * 4] = 255;
          if (!spanAbove && y > 0 && d[(x1 + (y - 1) * w) * 4] < threshold) {
            stack.push([
              x1, y - 1
            ]);
            spanAbove = 1;
          } else if (spanAbove && y > 0 && d[(x1 + (y - 1) * w) * 4] >= threshold) {
            spanAbove = 0;
          }
          if (!spanBelow && y < h - 1 && d[(x1 + (y + 1) * w) * 4] < threshold) {
            stack.push([
              x1, y + 1
            ]);
            spanBelow = 1;
          } else if (spanBelow && y < h - 1 && d[(x1 + (y + 1) * w) * 4] >= threshold) {
            spanBelow = 0;
          }
          x1++;
        }
      }
    }

    magnify () {
      const tempCanvas = document.createElement('canvas');
      drawStepToCanvas(this.terr, tempCanvas);
      return hqx(tempCanvas, this.options.blackToAlpha)
    }

    // Generate a terrain and return a html Canvas object representing it(red on black image)
    generate (seed$$1) {
      if (seed$$1 < 0 || seed$$1 >= 1) {
        throw new Error('Invalid seed: ' + seed$$1 + ', must be between [0,1).')
      }
      const debug = this.options.debug;
      if (debug) timer.start('generate-terrain');

      // Start with a copy of the terrain type image
      if (debug) timer.start('terrain-type');
      this.terr.data.set(this.terrainType.data);
      if (debug) timer.stop('terrain-type');
      if (debug) drawStepToCanvas(this.terr, 'canvas-terrain');

      // Extend terrain accross area defined by random Perlin noise
      if (debug) timer.start('perlin-noise');
      this.perlinNoise(seed$$1);
      if (debug) timer.stop('perlin-noise');
      if (debug) drawStepToCanvas(this.terr, 'canvas-perlin');

      if (debug) timer.start('floodfill-perlin');
      this.floodFill(this.terrainTypePoints.fg.slice(), {});
      if (debug) timer.stop('floodfill-perlin');
      if (debug) drawStepToCanvas(this.terr, 'canvas-fperlin');

      if (debug) timer.start('remove-perlin');
      this.removePerlin();
      if (debug) timer.stop('remove-perlin');
      if (debug) drawStepToCanvas(this.terr, 'canvas-rperlin');

      // Cleanup terrain shape through convolutions
      if (debug) timer.start('dilation');
      convolution(['dilation', 'dilation', 'dilation', 'dilation', 'dilation'], this.terr, this.terr);
      if (debug) timer.stop('dilation');
      if (debug) drawStepToCanvas(this.terr, 'canvas-dilation');

      if (debug) timer.start('nohole');
      this.paintBackground();
      if (debug) drawStepToCanvas(this.terr, 'canvas-paintbg');
      this.removeHoles();
      if (debug) timer.stop('nohole');

      if (debug) timer.start('erosion');
      convolution(['erosion', 'erosion', 'erosion', 'erosion'], this.terr, this.terr);
      if (debug) timer.stop('erosion');
      if (debug) drawStepToCanvas(this.terr, 'canvas-erosion');

      // Magnify result to final size using hq2x algorithm
      if (debug) timer.start('magnify');
      const finalCanvas = this.magnify();
      if (debug) timer.stop('magnify');
      if (debug) drawStepToCanvas(finalCanvas, 'canvas-magnify', 0.5);

      // Done!
      if (debug) timer.stop('generate-terrain');
      return finalCanvas
    }
  }

  const DEFAULT_OPTIONS$1 = {
    marginTop: 40,  // don't generate position too high
    marginRight: 1,
    marginBottom: 160,  // don't generate position in water
    marginLeft: 1,
    debug: false
  };

  // Use Halton sequence to generate positions
  // https://en.wikipedia.org/wiki/Halton_sequence
  // This will return a number between [0,1)
  function halton (index, base) {
    let result = 0;
    let f = 1 / base;
    let i = index;
    while (i > 0) {
      result = result + f * (i % base);
      i = Math.floor(i / base);
      f = f / base;
    }
    return result
  }

  // Square distance between 2 points
  function dist2 (a, b) {
    return Math.pow(b[0] - a[0], 2) + Math.pow(b[1] - a[1], 2)
  }

  // Generate pseudo-random(uniformly distributed) positions on a the surface of a given terrain
  class PositionGenerator {

    constructor (terrainShape, opts) {
      // Check options values
      this.options = Object.assign({}, DEFAULT_OPTIONS$1, opts);
      if (this.options.debug) timer.start('position-generator');

      const surfaceImg = new ImageData(terrainShape.width, terrainShape.height);
      convolution(['surface', 'threshold', 'surfaceErosion', 'surfaceErosion', 'surfaceErosion'], terrainShape, surfaceImg);

      // Compute the surface points of the terrain
      let surfacePoints = [];
      const w = surfaceImg.width;
      const h = surfaceImg.height;
      for (let y = this.options.marginTop; y < h - this.options.marginBottom; y++) {
        for (let x = this.options.marginLeft; x < w - this.options.marginRight; x++) {
          if (surfaceImg.data[(x + y * w) * 4] !== 0) {
            surfacePoints.push([x, y]);
          }
        }
      }

      // Sort points by location using a simple march-through algorithm
      // Start at bottom left and march through closest surfacePoints
      let curr = [0, h - 1];
      const visited = [];
      while (surfacePoints.length > 0) {
        // Sort remaining points by distance to current
        if (dist2(curr, surfacePoints[surfacePoints.length - 1]) > 25) {
          surfacePoints.sort((a, b) => dist2(curr, b) - dist2(curr, a));
        }
        curr = surfacePoints.pop();
        visited.push(curr);
      }

      this.surfacePoints = visited;
      this.haltonIndex = 0;
      if (this.options.debug) timer.stop('position-generator');
    }

    // Return next random surface point
    getSurfacePoint (seed) {
      let nextRandomNb = halton(this.haltonIndex++, 2);
      // randomize halton sequence by using the seed param as an offset(seed should be between [0,1))
      nextRandomNb = (nextRandomNb + seed) % 1.0;
      return this.surfacePoints[Math.floor(nextRandomNb * this.surfacePoints.length)]
    }

    drawSurfacePoints (terrainCanvas, scaleFactor) {
      const ctx = terrainCanvas.getContext('2d');
      ctx.fillStyle = 'rgba(255,0,0,1)';
      for (let curr = 0; curr < this.surfacePoints.length; curr++) {
        let currPt = this.surfacePoints[curr];
        ctx.fillRect(currPt[0]*scaleFactor, currPt[1]*scaleFactor - 1, 1, 2);
      }
    }
  }

  

  // Generate terrain graphics from a terrain shape
  class TerrainRenderer {

    // Factory method to create a TerrainRenderer from img urls instead of img objects
    static async fromImgUrl (shapeCanvas, opts) {
      const imgOpts = Object.assign({}, opts, {
        groundImg: await loadImage(opts.groundImg),
        backgroundImg: await loadImage(opts.backgroundImg),
        charaImg: await loadImage(opts.charaImg)
      });
      return new TerrainRenderer(shapeCanvas, imgOpts)
    }

    constructor (shapeCanvas, opts) {
      // Check option values
      this.options = Object.assign({}, DEFAULT_OPTIONS$2, opts);
      this.borderColor = hexToRgb(this.options.borderColor);
      if (!this.borderColor) {
        throw new Error('Invalid borderColor value. Should be hex color like #aa3300')
      }
      if (!this.options.groundImg || !this.options.backgroundImg || !this.options.charaImg) {
        throw new Error('Required groundImg, backgroundImg, and charaImg options must be HTMLImageElement(or CanvasImageSource)')
      }

      // Initalize properties
      this.terrainShape = shapeCanvas.getContext('2d').getImageData(0, 0, shapeCanvas.width, shapeCanvas.height);
      this.terr = new ImageData(shapeCanvas.width, shapeCanvas.height);
      this.posGenerator = new PositionGenerator(this.terrainShape, {debug: this.options.debug});

      // Read the background and ground images into ImageData objects
      this.background = getHtmlImageData(this.options.backgroundImg, { width: this.terr.width, height: this.terr.height });
    
        var imgGround = this.options.groundImg;
        
        console.log('imgGround', imgGround);
        
        var canvas = document.createElement("canvas");
        canvas.width = 400;
        canvas.height = 400;
        
        var ctx = canvas.getContext("2d");
        ctx.fillStyle = planet_surface_settings.ground;
        ctx.fillRect(0, 0, 400, 400);
        
        var context = canvas.getContext("2d");
        var pattern = context.createPattern(imgGround, 'repeat');
        context.rect(0, 0, canvas.width, canvas.height);
        context.fillStyle = pattern;
        context.fill();
        
        var img = document.createElement("img");
        
        img.src = canvas.toDataURL("image/png");
        
        console.log('img', img);
        
        
      //this.ground = getHtmlImageData(imgGround);
      this.ground = getHtmlImageData(img);
        console.log('this.options.groundImg', this.options.groundImg);
        console.log('this.ground', this.ground);
    }

    // Draw the terrain on specific canvas
    // NOTE: in a real game, background and foreground would be drawn separately at different times
    drawTerrain (seed, bgCanvas, bgWaterCanvas, fgCanvas, fgWaterCanvas) {
      if (seed < 0 || seed >= 1) {
        throw new Error('Invalid seed: ' + seed + ', must be between [0,1).')
      }
      const debug = this.options.debug;
      if (debug) timer.start('draw-terrain');
      const randomGen = new Random(seed);
        
        console.log('this', this);

      // Draw background
      bgCanvas.width = this.terr.width;
      bgCanvas.height = this.terr.height;
      bgCanvas.getContext('2d').putImageData(this.background, 0, 0);

      this.drawWave(bgWaterCanvas, this.terr.width, 160, 15);

      // Draw terrain
      this.groundOffsetX = randomGen.nextIntBetween(0, this.ground.width); // start at random X
      this.groundOffsetY = randomGen.nextIntBetween(0, this.ground.height); // start at random Y
      if (debug) timer.start('texturize');
      this.texturize();
      if (debug) timer.stop('texturize');

      // Draw result to fgCanvas
      const fgCtx = fgCanvas.getContext('2d');
      fgCanvas.width = this.terr.width;
      fgCanvas.height = this.terr.height;
      fgCtx.putImageData(this.terr, 0, 0);

      if (debug) {
        drawStepToCanvas(fgCanvas, 'canvas-render', 0.5);
        drawStepToCanvas(fgCanvas, 'canvas-surface', 0.5);
        this.posGenerator.drawSurfacePoints(document.getElementById('canvas-surface'), 0.5);
      }

      // Draw characters
      if (debug) timer.start('characters');
      this.drawCharacters(seed, randomGen, fgCtx);
      if (debug) timer.stop('characters');

      if (debug) drawStepToCanvas(fgCanvas, 'canvas-chara', 0.5);
      if (debug) timer.start('wave');
      this.drawWave(fgWaterCanvas, this.terr.width, 160, 21);
      if (debug) timer.stop('wave');

      if (debug) timer.stop('draw-terrain');
    }

    drawCharacters (seed, randomGen, canvasCtx) {
      // Here we expect charaImg to be a grid of characters that we choose from randomly
      const charaImg = this.options.charaImg;
      const charaW = this.options.charaWidth || charaImg.width;
      const charaH = this.options.charaHeight || charaImg.height;
      const charaCol = charaImg.width / charaW;
      const charaRow = charaImg.height / charaH;
      for (let n = 0; n < this.options.nbCharas; n++) {
        const pt = this.posGenerator.getSurfacePoint(seed);
        const choiceCharaRow = randomGen.nextIntBetween(0, charaRow - 1);
        const choiceCharaCol = randomGen.nextIntBetween(0, charaCol - 1);
        canvasCtx.drawImage(charaImg, choiceCharaCol * charaW, choiceCharaRow * charaH, charaW, charaH, pt[0] - charaW / 2, pt[1] - charaH + 10, charaW, charaH);
      }
    }

    // Wave code adapted from https://codepen.io/jeffibacache/pen/tobCk
    drawWave (waveCanvas, width, height, waveOffset) {
      waveCanvas.width = width;
      waveCanvas.height = height;
      if (waveCanvas.waveAnim) {
        cancelAnimationFrame(waveCanvas.waveAnim);
        waveCanvas.waveAnim = undefined;
      }
      const context = waveCanvas.getContext('2d');
      context.strokeStyle = this.options.waveColor;

      const fpsInterval = 1000 / this.options.waveFps;
      const waveDuration = this.options.waveDuration;
      const waveDisplacement = this.options.waveDisplacement;

      let offset = waveOffset;
      let drawFrame = (elapsed) => {
        context.clearRect(0, 0, width, height);
        context.beginPath();
        context.moveTo(0, waveOffset + Math.sin(offset) * Math.cos(1) + (height >> 1));

        for (let i = 0; i < Math.PI * 2; i += 0.4) {
          context.lineWidth = 140;
          context.lineTo((i / Math.PI * 2) * width, waveOffset + Math.sin(i + offset) * waveDisplacement + (height >> 1));
        }
        context.stroke();
        offset += (elapsed / 700.0);
      };

      // Drawloop is always called when rendering a frame but we draw
      // the waves with drawFrame() only at a specific framerate.
      let start = Date.now();
      let then = start;
      let now = null;
      let drawLoop = () => {
        if (waveDuration && now - start > waveDuration) return
        now = Date.now();
        waveCanvas.waveAnim = requestAnimationFrame(drawLoop);
        const elapsed = now - then;
        if (elapsed > fpsInterval) {
          then = now - (elapsed % fpsInterval);
          drawFrame(elapsed);
        }
      };
      drawLoop();
      drawFrame(1);
    }

    texturize () {
      const terrainShape = this.terrainShape.data;
      const terrain = this.terr.data;
      const w = this.terr.width;
      const h = this.terr.height;
      const ground = this.ground.data;
      const groundW = this.ground.width;
      const groundH = this.ground.height;
      const borderWidth = this.options.borderWidth;

      for (let y = 0; y < h; y++) {
        for (let x = 0; x < w; x++) {
          const pix = (x + y * w) * 4;

          if (terrainShape[3 + pix] === 0) {
            // Pixel is not terrain
            terrain[pix] = 0;
            terrain[1 + pix] = 0;
            terrain[2 + pix] = 0;
            terrain[3 + pix] = 0;
            continue
          }

          // Pixel is terrain
          terrain[3 + pix] = terrainShape[3 + pix];   // Copy alpha

          let isBorder = false;
          if (borderWidth >= 1) {
            for (let bw = 1; bw <= borderWidth; bw++) {
              if (terrainShape[(x + (y - bw) * w) * 4] === 0) {
                isBorder = true;
                break
              }
            }
          }

          if (isBorder) {
            // Pixel is on terrain top border
            terrain[pix] = this.borderColor.r;
            terrain[1 + pix] = this.borderColor.g;
            terrain[2 + pix] = this.borderColor.b;
            continue
          }

          // Pixel is inside terrain
          const groundPix = ((this.groundOffsetX + x) % groundW + ((this.groundOffsetY + y) % groundH) * groundW) * 4;
          terrain[pix] = ground[groundPix];
          terrain[1 + pix] = ground[1 + groundPix];
          terrain[2 + pix] = ground[2 + groundPix];

          if (terrainShape[(x + (y - borderWidth - 1) * w) * 4] === 0) {
            // Pixel is just below the terrain border
            // Use alpha from the shape border top pixel for blending this pixel too
            // This will antialiase the bottom edge of the terrain border
            const alpha = terrainShape[3 + (x + (y - borderWidth) * w) * 4] / 255.0;
            terrain[pix] = terrain[pix] * alpha + this.borderColor.r * (1.0 - alpha);
            terrain[1 + pix] = terrain[1 + pix] * alpha + this.borderColor.g * (1.0 - alpha);
            terrain[2 + pix] = terrain[2 + pix] * alpha + this.borderColor.b * (1.0 - alpha);
          }
        }
      }
    }
  }

  const minTerrainWidth = 880;
  const maxTerrainWidth = 2048;
  const clientWidth = document.documentElement.clientWidth;
  let options = {
    w: Math.max(Math.min(clientWidth, maxTerrainWidth), minTerrainWidth),
    seed: Math.random(),
    noise: 35,
    type: 'type-3',
    charas: planet_surface_settings.players
  };

  // Instantiate a new TerrainGenerator
  let terrainGenerator = null;
  async function newTerrainGenerator (settings) {
    toggleForm(false);
    await waitForUi();
    const width = options.w;
    const height = Math.floor(width * 0.625);
    console.log('options', options);
    console.log('settings', settings);
    terrainGenerator = await TerrainGenerator.fromImgUrl({
        debug: false,
        width: width - width % 2, // Make sure width is even
        height: height - height % 2, // Make sure height is even
        terrainTypeImg: './img/' + settings.type + '.png',
        waveColor: settings.waveColor,
        borderColor: settings.borderColor,
        waveFps: settings.waveFps,
        waveDuration: settings.waveDuration,
        noiseResolution: settings.noiseResolution,
        noiseResolutionBlack: settings.noiseResolutionBlack,
        noiseThreshold: settings.noiseThreshold,
        blackToAlpha: settings.blackToAlpha,
        nbCharas: settings.players
    });
    generateTerrain();
  }

  // Generate a new terrainShape
  let terrainShape = null;
  async function generateTerrain () {
    toggleForm(false);
    await waitForUi();
    timer.start('terrain-total');
    terrainShape = terrainGenerator.generate(options.seed);
    renderTerrain().then(() => {
      timer.stop('terrain-total');
      timer.toHtmlElts();
        
    });
  }

  // Render the current terrainShape
  async function renderTerrain () {
    if (!terrainShape) return
    toggleForm(false);
    const graphicsRenderer = await TerrainRenderer.fromImgUrl(terrainShape, {
      debug: true,
      groundImg: './img/dirt.png',
      backgroundImg: './img/background.svg',
      charaImg: './img/chara.png',
      charaWidth: 44,
      charaHeight: 41,
      nbCharas: options.charas
    });
    graphicsRenderer.drawTerrain(
      options.seed,
      document.getElementById('bgcanvas'),
      document.getElementById('bgwater'),
      document.getElementById('fgcanvas'),
      document.getElementById('fgwater')
    );
    document.getElementById('result').classList.remove('loading');

    if (history.pushState) {
      // display current terrain config to querystring for linking
      const url = window.location.protocol + '//' + window.location.host + window.location.pathname + '?terrain=' + btoa(JSON.stringify(options));
      window.history.pushState({path: url}, '', url);
    }
    toggleForm(true);
  }

  // Demo form management
  const formElts = ['gen', 'type1', 'type2', 'type3', 'noiseres'];
  function toggleForm (enabled) {
    formElts.map(elt => {
      document.getElementById(elt).disabled = !enabled;
    });
  }

  // This method just waits a bit to let the UI refresh
  // Ideally terrain stuff should be in a web worker instead but workers don't support Canvas API yet
  function waitForUi () {
    return new Promise(r => setTimeout(r, 20), err => console.log(err))
  }

  // Init form controls and stuff
  function pageInit () {
    for (let i = 0; i < document.genform.selshape.length; i++) {
      document.genform.selshape[i].onclick = function () {
        options.type = this.value;
        newTerrainGenerator();
      };
    }

    document.getElementById('noiseres').onchange = function () {
      options.noise = parseInt(this.value);
      newTerrainGenerator();
    };

    document.getElementById('nbcharas').onchange = function () {
      options.charas = parseInt(this.value);
      renderTerrain();
    };

    document.getElementById('gen').onclick = function () {
      options.seed = Math.random();
      generateTerrain();
      return false
    };

    // Load options from querystring config if any
    if (window.location.search.substring(0, 9) === '?terrain=') {
      options = JSON.parse(atob(window.location.search.substring(9)));
      options.w = Math.min(options.w, maxTerrainWidth);
    }

    // Adjust html size to match querystring config width
    if (options.w > clientWidth) {
      ['bgcanvas', 'bgwater', 'fgcanvas', 'fgwater'].map(canvas => {
        document.getElementById(canvas).style.maxWidth = '100%';
      });
    } else if (options.w < clientWidth) {
      const resultDivStyle = document.getElementById('bgcanvas').parentElement.style;
      resultDivStyle.width = options.w + 'px';
      resultDivStyle.height = Math.floor(options.w * 0.625) + 'px';
      resultDivStyle.paddingBottom = '0px';
    }

    // Make the generate terrain menu sticky after scroll
    const genform = document.getElementById('genform');
    const initOffset = genform.offsetTop;
    window.onscroll = () => {
      genform.classList.toggle('sticky', window.pageYOffset >= initOffset);
    };

    // Everything is done, start generating a terrain
    newTerrainGenerator(planet_surface_settings);
  }

  pageInit();

}());
