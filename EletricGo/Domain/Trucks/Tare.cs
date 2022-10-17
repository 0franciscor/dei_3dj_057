using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class Tare : IValueObject<Tare>
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

        public float AsFloat()
        {
            return tare;
        }

    }    
}