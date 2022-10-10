using System;
using src.Domain.Shared;
using Newtonsoft.Json;

namespace src.Domain.Warehouse
{
    public class WarehouseDesignation : ValueObject
    {
        private String designation;

        [JsonConstructor]
        public WarehouseDesignation(Guid value) : base(value)
        {
        }

        public WarehouseDesignation(String value) : base(value)
        {
        }

       override String toString(){
            return "Warehouse Designation:" + designation;
       }

        override boolean equals(Object obj ){
            if ( this == obj){
                return true;
            }

            if(!(obj.GetType == typeof(WarehouseDesignation) )){
                return false;
            }

            WarehouseDesignation whDes = (WarehouseDesignation) obj;
            return this.designation.Equals(whDes.designation);
        }
    
         override int hashCode(){
            return this.designation.GetHashCode();
         }
    }
}