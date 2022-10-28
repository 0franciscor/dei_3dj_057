using EletricGo.Domain.Deliveries;
using EletricGo.Infrastructure.Deliveries;
using Microsoft.EntityFrameworkCore;

namespace EletricGo.Infrastructure
{
    public class EletricGoDBContext : DbContext
    {
        public DbSet<Delivery> Delivery { get; set; }

        public EletricGoDBContext(DbContextOptions options) : base(options)
        {
        }


        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new DeliveryEntityTypeConfiguration());
        }
    }
    
}