using EletricGo.Domain.Shared;
using System;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryID : EntityID
    {
        private string deliveryID { get;}

        public DeliveryID(String value) : base(value)
        {
            this.deliveryID = value;
        }
        
        override
        public String AsString(){
            return deliveryID;
        }

        override
        public bool Equals(Object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            DeliveryID delivery = (DeliveryID)obj;
            return deliveryID == delivery.deliveryID;
        }

    }    
}