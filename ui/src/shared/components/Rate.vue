<template>
  <div>
    <el-input v-if='rateObj.tag == "Periodic"' v-model.number="rateObj.contents" type="number" placeholder="Rate"></el-input>
    <el-date-picker v-if='rateObj.tag == "OneTime"'
      v-model="rateObj.contents"
      type="date"
      format="yyyy-MM-dd"
      placeholder="Date">
    </el-date-picker>
    <el-select v-model="rateObj.tag" placeholder="Select Rate Type" v-on:change="emitChange">
      <el-option v-for="(item,index) in rateTypes" :key="index" :value="item" :label="item">
      </el-option>
    </el-select>
  </div>
</template>

<script>
  export default {
    props: ['rate'],
    data () {
      return {
        rateObj: this.rate,
        rateTypes: ["OneTime","Periodic"]
      }
    },
    methods: {
      emitChange() {
        this.$emit('update:rate', this.rateObj);
      }
    }
  }
</script>
