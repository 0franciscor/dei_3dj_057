using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class LoadTime : ValueObject
    {

        private float time;

        public LoadTime(float time)
        {
            this.time = time;
        }

        public String toString()
        {
            return time.ToString();
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
            return time;
        }

    }    
}