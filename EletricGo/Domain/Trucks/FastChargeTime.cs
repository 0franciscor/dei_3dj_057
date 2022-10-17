using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class FastChargeTime : IValueObject<FastChargeTime>
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

        public float AsFloat()
        {
            return fastChargeTime;
        }

    }
}