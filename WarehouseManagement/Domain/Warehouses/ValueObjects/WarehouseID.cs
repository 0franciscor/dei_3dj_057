using System;
using System.Text.RegularExpressions;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Warehouses.ValueObjects
{
	public class WarehouseId : EntityID
	{
		
		public string warehouseID { get;}

		public WarehouseId(string value) : base(value)
		{
			
			this.warehouseID = value;
		}


		override
		public string AsString()
		{
			Guid obj = (Guid)base.ObjValue;
			return obj.ToString();
		}

		public Guid AsGuid()
		{
			return (Guid)base.ObjValue;
		}

	}
}