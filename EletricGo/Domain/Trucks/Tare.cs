using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class Tare : ValueObject
    {

        private float tare;

        public Tare(float tare)
        {
            this.tare = tare;
        }

        public String toString()
        {
            return tare.ToString();
        }
        
        public Boolean equals(Object obj)
        {
            return true;
        }
        
        public int hashCode()
        {
            return 0;
        }

    }    
}