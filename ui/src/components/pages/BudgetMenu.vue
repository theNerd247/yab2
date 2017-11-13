<template>
	<el-col :offset="8" :span="8">
		<el-card>
			<div slot="header">
				<span>Select Budget</span>
				<router-link :to="bname" append style="float: right; padding: 3px 0" type="text">View Budget</router-link>
			</div>
			<BudgetNames :budgetName.sync="bname"></BudgetNames>
		</el-card>
	</el-col>
</template>

<script>

	import BudgetNames from '@/shared/components/BudgetNames.vue'

	export default {
		components: { BudgetNames },
		data () {
			return {
				bname: ''
			}
		}
	}

</script>
