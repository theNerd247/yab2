<template>
<home-card title="Upload Transactions">
	<el-form slot="content" inline>
	<el-form-item>
		<el-input v-model="name" placeholder="Budget Name"></el-input>
	</el-form-item>
	<el-form-item>
    <el-upload 
      class="upload-demo" 
      ref="upload"
      :action="uploadURL"
      :auto-upload="false"
      >
      <el-button slot="trigger"> <i class="el-icon-upload"></i> Add File </el-button>
      </el-upload>
  </el-form-item>
  <el-form-item>
    <el-button @click="uploadTransaction" type="info" style="float: right;"><i class="el-icon-upload"></i> Upload</el-button>
  </el-form-item>
	</el-form>
</home-card>
</template>

<script>
import Vue from 'vue'
import HomeCard from './HomeCard.vue'
import budgetsJSON from '@/assets/budgets.json'
import { baseURL } from '@/shared/http-common'

export default {
  components: {
    HomeCard,
	},
	data  () {
		return {
      name: '',
		}
	},
  computed: {
    uploadURL() {
      return '/transaction/'+this.name;
    }
  },
  methods: {
    uploadTransaction () {
      this.$refs.upload.submit();
    }
  }
}
</script>
