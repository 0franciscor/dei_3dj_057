using System;
using src.Domain.Shared;
using Newtonsoft.Json;

namespace src.Domain.Warehouse
{
    public class Warehouse : EntityId
    {
        [JsonConstructor]
        public WarehouseId(Guid value) : base(value)
        {
        }

        public WarehouseId(String value) : base(value)
        {
        }

        override
        protected  Object createFromString(String text){
            return new Guid(text);
        }
        
        override
        public String AsString(){
            Guid obj = (Guid) base.ObjValue;
            return obj.ToString();
        }
        public Guid AsGuid(){
            return (Guid) base.ObjValue;
        }
    }
}