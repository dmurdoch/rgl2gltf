rglwidgetClass.prototype.rgl2gltfAnimation = function(el, control) {

  var value = control.value, animation = control.animation,
  i, channel, sampler, input, output, obj, subscenes, M, change;
  if (rglwidgetClass.missing(value))
    value = control.value = 0;
  subscenes = [];
  for (i = 0; i < animation.channels.length; i++) {
    channel = animation.channels[i];
    sampler = animation.samplers[channel.sampler];
    input = this.readAccessor(sampler.input, control.buffer);
    output = this.readAccessor(sampler.output, control.buffer);
    obj = this.getObj(channel.target.node);
    if (subscenes.indexOf(obj.id) < 0)
      subscenes.push(obj.id);
    if (typeof obj.change === "undefined")
      obj.change = {};
    switch(channel.target.path) {
      case "rotation":
        obj.change.rotation = rglwidgetClass.slerp(input, output, value);
        break;
      case "translation":
        obj.change.translation = rglwidgetClass.lerp(input, output, value);
        break;
      case "scale":
        obj.change.scale = rglwidgetClass.lerp(input, output, value);
        break;
    }
  }
  for (i = 0; i < subscenes.length; i++) {
    obj = this.getObj(subscenes[i]);
    M = new CanvasMatrix4();
    change = obj.change.scale;
    if (typeof change !== "undefined")
      M.scale(change[0], change[1], change[2]);
    change = obj.change.rotation;
    if (typeof change !== "undefined")
      rglwidgetClass.rotateByQuaternion(M, change);
    change = obj.change.translation;
    if (typeof change !== "undefined")
      M.translate(change[0], change[1], change[2]);
    obj.par3d.userMatrix = M;
  }

};

rglwidgetClass.prototype.rgl2gltfWeighted = function(el, control) {
  var sub = this.getObj(control.subid),
    result = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0], M, i, j, node;
  if (typeof control.initialized === "undefined") {
    control.weights = [].concat(control.weights);
    control.nodes   = [].concat(control.nodes);
    for (i = 0; i < control.weights.length; i++) {
      M = new CanvasMatrix4();
      M.load(control.backtransform[i]);
      control.backtransform[i] = M;
    }
    control.initialized = true;
  }
  for (i = 0; i < control.weights.length; i++) {
    node = this.getObj(control.nodes[i]);
    M = new CanvasMatrix4(node.forward);
    M.multLeft(control.backtransform[i]);
    M = M.getAsArray();
    for (j = 0; j < 16; j++)
      result[j] += control.weights[i]*M[j];
  }
  sub.par3d.userMatrix.load(result);
};

rglwidgetClass.prototype.rgl2gltfSkeleton = function(el, control) {
  var self = this,
      recurse = function(subid, transform) {
        var obj = self.getObj(subid), i;
        obj.forward = new CanvasMatrix4(transform);
        obj.forward.multLeft(obj.par3d.userMatrix);
        for (i = 0; i < obj.subscenes.length; i++) {
          recurse(obj.subscenes[i], obj.forward);
        }
      };

  recurse(control.subid, new CanvasMatrix4());

};

rglwidgetClass.prototype.rgl2gltfShaderUniforms = function(el, control) {
  var obj = this.getObj(control.id),
      joints, i, j, joint, M;
  if (typeof control.initialized === "undefined") {
    control.joints   = [].concat(control.joints);
    for (i = 0; i < control.backtransform.length; i++) {
      M = new CanvasMatrix4();
      M.load(control.backtransform[i]);
      control.backtransform[i] = M;
    }
    control.initialized = true;
  }
  joints = control.joints;
  for (i = 0; i < joints.length; i++) {
    joint = this.getObj(joints[i]);
    M = new CanvasMatrix4(joint.forward);
    M.multLeft(control.backtransform[i]);
    M = M.getAsArray();
    for (j = 0; j < 4; j++)
      obj.userUniforms.uJointMat[4*i + j] = M.slice(4*j, 4*j+4);
  }
};
