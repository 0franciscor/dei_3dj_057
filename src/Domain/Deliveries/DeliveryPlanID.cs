using Shared;

namespace src.Domain.Deliveries
{
    public class DeliveryPlanID : EntityId
    {

        public DeliveryPlanID(String value):base(value)
        {

        }

        override
        protected  Object createFromString(String text){
            return text;
        }
        override
        public String AsString(){
            return (String) base.Value;
        }
    }
}