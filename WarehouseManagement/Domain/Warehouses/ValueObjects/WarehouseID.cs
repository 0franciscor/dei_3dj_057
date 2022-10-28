using WarehouseManagement.Shared;
using System;

namespace WarehouseManagement.Warehouses.ValueObjects
{
	public class WarehouseID : EntityID
	{
		
		private string deliveryID { get; set; }

		public WarehouseID(String value) : base(value)
		{
			this.deliveryID = value;
		}

		override
		protected Object createFromString(String text)
		{
			return new Guid(text);
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