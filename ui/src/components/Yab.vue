<template>
	<el-row :gutter="20">
		<el-col :span="24">
			<el-row>
				<el-menu theme="dark" :default-active="activeIndex" class="el-menu-demo" mode="horizontal" @select="handleSelect">
					<el-menu-item index="1">YAB</el-menu-item>
				</el-menu>
			</el-row>

      <el-row>
        <router-view/>
      </el-row>
		</el-col>
	</el-row>
</template>
