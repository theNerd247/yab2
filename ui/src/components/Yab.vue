<style>
  .collapsed {
    max-width: max-content;
  }
</style>

<template>
  <el-container>
    <el-aside v-bind:class="{collapsed: isCollapsed}">
      <el-row>
        <el-col>
          <!--<el-button round style="padding: 8px 8px;" @click="toggleCollapse()">-->
            <!--<i v-if="isCollapsed" class="el-icon-arrow-right"></i>-->
            <!--<i v-if="!isCollapsed" class="el-icon-arrow-left"></i>-->
          <!--</el-button>-->
        </el-col>
      </el-row>
      <el-row>
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
      </el-row>
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
