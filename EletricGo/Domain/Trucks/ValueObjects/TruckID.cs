using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Trucks.ValueObjects
{
    public class TruckID : EntityID
    {


        public TruckID(Guid value) : base(value)
        {

        }

        public TruckID(String value) : base(value)
        {
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
