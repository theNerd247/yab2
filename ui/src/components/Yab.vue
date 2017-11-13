<template>
	<el-container>
		<el-aside>
			<el-menu mode="vertical" :router="true">
				<el-menu-item index="/">YAB</el-menu-item>
				<el-menu-item index="/budget">
					<i class="el-icon-tickets"></i>
					<span slot="title">Budget</span>
				</el-menu-item>
			</el-menu>
		</el-aside>

			<el-main>
				<router-view/>
			</el-main>
	</el-container>
</template>

<script>
	import Vue from 'vue'

export default {
	data () {
		return {
		}
	}
}
</script>
