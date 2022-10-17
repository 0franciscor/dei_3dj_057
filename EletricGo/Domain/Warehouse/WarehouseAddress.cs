using System;
using EletricGo.Domain.Shared;
//using Newtonsoft.Json;

namespace EletricGo.Domain.Warehouse
{
    public class WarehouseAddress : IValueObject<WarehouseAddress>
    {
        public string address { get; }

        public WarehouseAddress(string address)
        {
            if (string.IsNullOrEmpty(address))
                throw new ArgumentNullException("value");
            this.address = address;
        }

        public String toString(){
            return address.ToString();
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