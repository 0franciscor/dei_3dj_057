using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class FastChargeTime : ValueObject
    {
        private float fastChargeTime;

        public FastChargeTime(float fastChargeTime)
        {
            this.fastChargeTime = fastChargeTime;
        }

        public String toString()
        {
            return fastChargeTime.ToString();
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