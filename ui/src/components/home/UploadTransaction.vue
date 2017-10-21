<template>
<home-card title="Upload Transactions">
	<el-button slot="action" @click="uploadTransaction" type="info" style="float: right;"><i class="el-icon-upload"></i> Upload</el-button>
	<el-form slot="content">
	<el-form-item>
		<el-input placeholder="Budget Name"></el-input>
	</el-form-item>
	<el-form-item>
		<el-col :span="16" :offset="4">
			<el-upload class="upload-demo" drag action="https://jsonplaceholder.typicode.com/posts/" multiple>
				<i class="el-icon-upload"></i>
					<div class="el-upload__text">Drop file here or <em>click to upload</em></div>
				</el-upload>
			</el-col>
		</el-form-item>
	</el-form>
</home-card>
</template>

<script>
import Vue from 'vue'
import HomeCard from '@/shared/HomeCard.vue'
import budgetsJSON from '@/assets/budgets.json'

export default {
  components: {
    HomeCard,
	},
	data  () {
		return {
		}
	},
}
</script>
