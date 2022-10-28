using EletricGo.Domain.Shared;
using System;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryID : EntityID
    {

        public string deliveryID { get; set; }

        public DeliveryID(String value) : base(value)
        {
            this.deliveryID = value;
        }

        override
        protected Object createFromString(String text){
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