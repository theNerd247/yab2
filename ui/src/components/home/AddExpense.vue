<template>
	<home-card title="Add Expense">
		<el-button slot="action" @click="createNewExpense" type="info" style="float: right;"><i class="el-icon-plus"></i> Add</el-button>
		<el-form slot="content">
			<el-form-item>
				<el-input v-model="newExpense.name" placeholder="Budget Name"></el-input>
			</el-form-item>
			<el-form-item>
				<el-input v-model="newExpense.reason" placeholder="Reason"></el-input>
			</el-form-item>
			<el-form-item>
				<el-input v-model="newExpense.amount" placeholder="Amount">
					<span slot="prepend">$</span>
				</el-input>
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
			expenses: [],
			newExpense: {
				name: '',
				amount: null,
				reason: ''
			}
		}
	},
	methods: {
		createNewExpense () { },
	}
}
</script>
