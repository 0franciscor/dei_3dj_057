using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryDate : ValueObject
    {

        private DateTime dateTime;

        public DeliveryDate(DateTime dateTime)
        {
            this.dateTime = dateTime;
        }

        public String toString()
        {
            return dateTime.ToString();
        }
        
        public Boolean equals(Object obj)
        {
            return true;
        }
        
        public int hashCode()
        {
            return 0;
        }

        public DateTime AsDateTime()
        {
            return dateTime;
        }
    }    
}