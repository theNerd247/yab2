<template>
	<div>
		<el-table :data="tableData.items">
			<el-table-column label="" fixed>
				<template slot-scope="scope">
					<el-button @click="httpUpdateItem(scope.$index)">Update</el-button>
					<el-button @click="httpRemoveItem(scope.$index)">Delete</el-button>
				</template>
			</el-table-column>
			<slot></slot>
		</el-table>
		<el-pagination
			@current-change="handleCurrentChange"
			:current-page.sync="curPage"
			:page-sizes="[50, 100, 200]"
			:page-size="count"
			layout="total, sizes, prev, pager, next, jumper"
			:total="total"
			>
		</el-pagination>
	</div>
</template>

<script>
	import { HTTP, httpWithNotify } from '@/shared/http-common.js'

export default {
	props: ['url','itemUrl','tdata'],
	data() {
		return{
			count: 50,
			curPage: 0,
			offset: 0,
			total: 0,
			tableData: this.tdata
		}
	},
	created() {
		this.httpGetData();
	},
	methods:{
		httpGetData(){
			let q = this.url;
			let params = {
				offset: this.offset,
				count: this.count
			};
			httpWithNotify(
				'',
				'Could not get expenses',
				HTTP.get(q, { params }),
				true
			).then(d => {
				this.tableData = d;
				this.total = this.tableData.items.length;
				this.emitChange();
			});
		},
		updateData() {
			let q = this.url;
			let t = this.tableData;
			httpWithNotify(
				'Updated!',
				'Could not update',
				HTTP.put(q, t)
			);

			this.emitChange();
		},
		httpUpdateItem(index){
			let item = this.tableData.items[index];
			let q = this.url + "/" + this.itemUrl +"/id/" + item.id;
			let i = item;
			httpWithNotify(
				'Updated Item',
				'Could not update item',
				HTTP.put(q,i)
			);
			this.emitChange();
		},
		httpRemoveItem(index){
			let item  = this.tableData.items[index];
			let q = this.url + "/" + this.itemUrl +"/id/" + item.id;
			httpWithNotify(
				'Item deleted',
				'Could not delete item' ,
				HTTP.delete(q)
			);
			this.emitChange();
		},
		httpAddItem(){
			let q = this.url + "/" + this.itemUrl;
			let newItem = httpWithNotify(
				'Added Item!',
				'Could not add item',
				HTTP.put(q)
			);

			this.tableData.items.push(newItem);
			this.emitChange();
		},
		handleCurrentChange(){
			this.offset = this.curPage*this.count;
		},
		emitChange(){
			this.$emit('update:tdata', this.tableData);
		}
	}
}
</script>
