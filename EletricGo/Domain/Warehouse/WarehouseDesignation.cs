using System;
using EletricGo.Domain.Shared;
//using Newtonsoft.Json;

namespace EletricGo.Domain.Warehouse
{
    public class WarehouseDesignation : IValueObject<WarehouseDesignation>
    {
        public string designation { get; }

        public WarehouseDesignation(string designation)
        {
            if (string.IsNullOrEmpty(designation))
                throw new ArgumentNullException("value");
            this.designation = designation;
        }
public String toString()
        {
            return designation.ToString();
        }
        
        public Boolean equals(Object obj)
        {
            return true;
        }
        
        public int hashCode()
        {
            return 0;
        }
    }
}