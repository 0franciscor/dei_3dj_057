using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryID : EntityID
    {

        private Guid deliveryID;

         public DeliveryID(Guid value) : base(value)
        {
            this.deliveryID = value;
        }

        public DeliveryID(String value) : base(value)
        {
            this.deliveryID = (Guid)createFromString(value);
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