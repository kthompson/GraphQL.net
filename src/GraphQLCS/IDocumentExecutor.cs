namespace GraphQL
{
    public interface IDocumentExecutor<in T>
    {
        void Execute(Document document, T context);
    }
}