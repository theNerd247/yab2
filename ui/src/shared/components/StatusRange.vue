<template>
  <el-row>
      <h3>Status</h3>
      <el-row style="width: max-content; height: max-content" type="flex">
        <el-col>
          <el-progress type="circle" :percentage="stat" :status="getStatus"></el-progress>
        </el-col>
        <el-col :span="23">
          <el-row style="height: 50%">
            <el-col>
              Budget: ${{ status[1]/100 }}
            </el-col>
          </el-row>
          <el-row style="height: 50%">
            <el-col>
              Current: ${{ status[0]/100 }}
            </el-col>
          </el-row>
        </el-col>
      </el-row>
  </el-row>
</template>

<script>
  import Vue from 'vue'

export default {
  props: ['status'],
  computed: {
    stat () {
      try{
        return Math.floor((this.status[1] - this.status[0])/this.status[1]*100);
      }
      catch(e)
      {
        return 0;
      }
    },
    getStatus () {
      if(this.stat >= 0)
        return "";
      else
        return "exception";
    },
  }
}
</script>
