using System;
using EletricGo.Domain.Shared;
//using Newtonsoft.Json;

namespace EletricGo.Domain.Warehouse
{
    public class WarehouseAddress : ValueObject<WarehouseAddress>
    {
        public string Value { get; private set; }

        public WarehouseAddress(string value)
        {
            if (string.IsNullOrEmpty(value))
                throw new ArgumentNullException("value");
            this.Value = value;
        }

        protected override bool EqualsCore(WarehouseAddress other)
        {
            return this.Value == other.Value;
        }

        protected override int GetHashCodeCore()
        {
            return this.Value.GetHashCode();
        }
    }
    {
        
    }
}