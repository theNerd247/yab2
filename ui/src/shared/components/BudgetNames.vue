<template>
  <el-select v-model="budgetName" placeholder="Budget Name" v-on:change="emitChange">
    <el-option v-for="(item,index) in budgetNames" :key="index" :value="item" :label="item">
    </el-option>
  </el-select>
</template>

<script>
import { HTTP } from '@/shared/http-common'

export default {
  props: ['budgetName'],
  data () {
    return {
      budgetNames: [],
			bname: this.budgetName
    }
  },
  created () {
    HTTP.get("budget-list/names")
      .then(response => {
        this.budgetNames = response.data.items;
				if(this.budgetNames.length > 0)
				{
					this.bname = this.budgetNames[0];
					this.emitChange();
				}
      });
  },
  methods: {
    emitChange() {
      this.$emit('update:budgetName', this.bname);
    }
  }
}
</script>
