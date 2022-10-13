using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryPlanID : EntityID
    {
        private Guid deliveryPlanID;

        public DeliveryPlanID(String value) : base(value)
        {
            this.deliveryPlanID = (Guid)createFromString(value);
        }

        public DeliveryPlanID(Guid value) : base(value)
        {
            this.deliveryPlanID = value;
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