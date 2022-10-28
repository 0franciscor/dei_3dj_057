using System.Threading.Tasks;

namespace EletricGo.Domain.Shared
{
    public interface IUnitOfWork
    {
        Task<int> CommitAsync();
    }
}