using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Trucks 
{
    public class TruckID : EntityID
    {

        private Guid truckID;

        public TruckID(Guid value) : base(value)
        {
            this.truckID = value;
        }

        public TruckID(String value) : base(value)
        {
            this.truckID = (Guid)createFromString(value);
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
