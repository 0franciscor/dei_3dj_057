using System;
using EletricGo.Domain.Shared;
//using Newtonsoft.Json;

namespace EletricGo.Domain.Warehouse
{
    public class WarehouseDesignation : IValueObject<WarehouseDesignation>
    {
        public string Value { get; private set; }

        public WarehouseDesignation(string value)
        {
            if (string.IsNullOrEmpty(value))
                throw new ArgumentNullException("value");
            this.Value = value;
        }

        protected override bool EqualsCore(WarehouseDesignation other)
        {
            return this.Value == other.Value;
        }

        protected override int GetHashCodeCore()
        {
            return this.Value.GetHashCode();
        }
    }
}