using System;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Warehouse
{
    public class WarehouseID : EntityID
    {
        //[JsonConstructor]
        public WarehouseID(Guid value) : base(value)
        {
        }

        public WarehouseID(String value) : base(value)
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