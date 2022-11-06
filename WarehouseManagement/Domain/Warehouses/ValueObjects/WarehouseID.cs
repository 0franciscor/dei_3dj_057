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
			if (string.IsNullOrEmpty(value))
			{
				throw new BusinessRuleValidationException("The Id can't be null or empty");
			}
			if (value.Length != 3)
			{
				throw new BusinessRuleValidationException("The Id must have only three characters");
			}
			
			
			if (int.TryParse(value, out _))
			{
				throw new BusinessRuleValidationException("The Id must be alphanumeric");
			}

			if (Regex.IsMatch(value, @"^[a-zA-Z]+$"))
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