<template>
  <div>
    <span>{{ formatRangeTip(status[0]) }}</span>
    <el-slider
      v-model="stat"
      :format-tooltip="formatRangeTip"
      range
      :max="statusMax"
      :min="statusMin"
      :step="100"
      >
    </el-slider>
    <span>{{ formatRangeTip(status[1]) }}</span>
  </div>
</template>

<script>
  import Vue from 'vue'

export default {
  props: ['status'],
  data () {
    return {
      stat: this.status
    }
  },
  computed: {
    statusMax () {
      return Math.max(this.stat[0], this.stat[1])+10000;
    },
    statusMin () {
      return Math.min(this.stat[0], this.stat[1])-10000;
    },
  },
  methods: {
    formatRangeTip (x) {
      return "$ " + x/100;
    },
  }
}
</script>
