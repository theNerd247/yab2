<style>
  .card-content {
    padding: 8px 8px;
  }
  .clearfix:before,
  .clearfix:after {
      display: table;
      content: "";
  }
  .clearfix:after {
      clear: both
  }

</style>

<template>
  <el-card :body-style="{ padding: '0px' }">
    <div slot="header" class="clearfix">
      <span>{{ title }}</span>
      <slot name="action">
      </slot>
    </div>
    <div class='card-content'>
      <slot name="content"></slot>
    </div>
   </el-card>
</template>

<script>
export default {
  name: 'HomeCard',
  props: ['title'],
  data () {
    return {
    }
  },
}
</script>
