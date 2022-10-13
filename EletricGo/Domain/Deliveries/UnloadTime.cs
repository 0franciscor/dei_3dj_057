using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class UnloadTime : ValueObject
    {

        private float time;

        public UnloadTime(float time)
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