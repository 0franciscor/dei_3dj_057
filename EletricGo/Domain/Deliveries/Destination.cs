using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class Destiantion : ValueObject
    {

        private String destination;

        public Destiantion(String destination)
        {
            this.destination = destination;
        }

        public String toString()
        {
            return destination;
        }
        
        public Boolean equals(Object obj)
        {
            return true;
        }
        
        public int hashCode()
        {
            return 0;
        }

        public String AsString()
        {
            return destination;
        }

    }    
}