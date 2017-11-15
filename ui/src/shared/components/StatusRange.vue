<style>

  .lowBudget .el-slider__button {
    border-color: #FA5555 !important;
  }

  .lowBudget .el-slider__bar {
    background-color: #FA5555 !important;
  }

  .highBudget .el-slider__button {
    border-color: #67C23A !important;
  }

  .highBudget .el-slider__bar {
    background-color: #67C23A !important;
  }

</style>
<template>
  <el-row>
    <el-row type="flex" justify="center">
      <el-col :span="8">
        Budget: ${{ status[0] }}
      </el-col>
      <el-col :span="8">
        Available: ${{ status[1] - status[0] }}
      </el-col>
      <el-col :span="8">
        Current: ${{ status[1] }}
      </el-col>
    </el-row>
    <el-row>
      <el-col>
        <el-slider disabled :class="{lowBudget: isLow, highBudget: !isLow}" v-model="stat" :max="statMax" :min="statMin" range :format-tooltip="format"></el-slider>
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
      return this.status.map(x => x*100);
    },
    isLow () {
      return this.status[1] < this.status[0];
    },
    statMax () {
      return Math.max(this.status[0], this.status[1])*100+5000;
    },
    statMin () {
      return Math.min(this.status[0], this.status[1])*100-5000;
    },
    getStatus () {
      if(this.stat >= 100)
        return "success";
      else if(this.stat > 0)
        return "";
      else
        return "exception";
    },
  },
  methods: {
    format(x) {
      return "$"+x/100;
    }
  }
}
</script>
