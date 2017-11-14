<style>
	.collapsed {
		max-width: max-content;
	}

	.navbar {
	}

</style>

<template>
	<el-container>
		<el-header class="navbar">
			<el-row>
				<el-col :span="1">
					<h2>
					<el-button round style="padding: 8px 8px;" @click="toggleCollapse()">
						<i v-if="isCollapsed" class="el-icon-arrow-right"></i>
						<i v-if="!isCollapsed" class="el-icon-arrow-left"></i>
					</el-button>
					</h2>
				</el-col>
				<el-col :span="23">
					<h2>
					YAB - Yet Another Budget
					</h2>
				</el-col>
			</el-row>
		</el-header>
		<el-container>
			<el-aside v-bind:class="{collapsed: isCollapsed}">
				<el-menu mode="vertical" :router="true" :collapse="isCollapsed">
					<el-menu-item index="/">
						<i class="el-icon-menu"></i>
						<span slot="title">Home</span>
					</el-menu-item>
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
	</el-container>
</template>

<script>
	import Vue from 'vue'

export default {
	data () {
		return {
			isCollapsed: true
		}
	},
	methods: {
		toggleCollapse () {
			this.isCollapsed = !this.isCollapsed;
		}
	}
}
</script>
