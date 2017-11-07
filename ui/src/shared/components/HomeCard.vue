<style>
	.action {
		float: right;
		padding: 3px 0;
	}
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
			<div class="action">
				<slot name="action">
				</slot>
			</div>
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
