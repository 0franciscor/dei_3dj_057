using System;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Warehouses.ValueObjects
{
	public class WarehouseID : EntityID
	{
		
		public string deliveryID { get;}

		public WarehouseID(String value) : base(value)
		{
			this.deliveryID = value;
		}


		override
		public String AsString()
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