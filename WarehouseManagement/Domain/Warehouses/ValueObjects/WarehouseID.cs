using System;
using System.Text.RegularExpressions;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Warehouses.ValueObjects
{
	public class WarehouseID : EntityID
	{
		
		public string warehouseID { get;}

		public WarehouseID(string value) : base(value)
		{
			if (value.Length != 3)
			{
				throw new BusinessRuleValidationException("The Id must have only three characters");
			}

			if (!Regex.IsMatch(value, "^[a-zA-Z0-9]*$"))
			{
				throw new BusinessRuleValidationException("The Id must be alphanumeric");
			}
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