loadonmortgage<-function(date="today")
{
  if (date=="today")
  {aa<-as.character(format(Sys.Date(),"%Y%m%d"))}else{aa<-as.character(date)}
  download.file(paste("ftp://yunying:YYftp%232015@172.16.6.238/","�������ſ���ʽ�ձ�",as.character(format(Sys.Date(),"%Y%m%d")),".CSV",sep=""),
                paste("�������ſ���ʽ�ձ�",as.character(format(Sys.Date(),"%Y%m%d")),".CSV",sep=""))
  a<-read.csv(paste("�������ſ���ʽ�ձ�",aa,".CSV",sep=""),stringsAsFactors = F)
  # ��֤δ�ύ��
  b<-subset(a,a$�ſ�������ʽ=="�ռ��վ�"&a$��ǰ����״̬!="�᰸")
  c<-ddply(b,.(��������,����),summarize,number=length(�������))
  colnames(c)<-c("��������","����","��֤δ�ύ��")
  # �쳣�᰸��
  b1<-subset(a,a$�ſ�������ʽ=="�ռ��վ�"&a$�ϴ�.ѺƷ�쳣������ͱ�.��������!="")
  b1<-ddply(b1,.(����),summarize,number=length(�������))
  if (length(b1$����)==0) {
    b1<-matrix(ncol = 2)
    colnames(b1)<-c("����","�쳣�᰸��")
  }
  # ����δ�ջ�
  b2<-subset(a,a$�ſ�������ʽ=="�ռ��վ�"&a$��ǰ����״̬!="�᰸")
  c1<-subset(b2,b2$��ʱ����>1&b2$��ʱ����<=5)
  c2<-subset(b2,b2$��ʱ����>5&b2$��ʱ����<=10)
  c3<-subset(b2,b2$��ʱ����>10&b2$��ʱ����<=20)
  c4<-subset(b2,b2$��ʱ����>20)
  c11<-ddply(c1,.(����),summarize,number=length(�������))
  if (length(c11$����)==0) {
    c11<-matrix(ncol = 2)
    colnames(c11)<-c("����","����5��������")
  }
  c21<-ddply(c2,.(����),summarize,number=length(�������))
  if (length(c21$����)==0) {
    c21<-matrix(ncol = 2)
    colnames(c21)<-c("����","����6-10����")
  }
  c31<-ddply(c3,.(����),summarize,number=length(�������))
  if (length(c31$����)==0) {
    c31<-matrix(ncol = 2)
    colnames(c31)<-c("����","����11-20����")
  }
  c41<-ddply(c4,.(����),summarize,number=length(�������))
  if (length(c41$����)==0) {
    c41<-matrix(ncol = 2)
    colnames(c41)<-c("����","����20��������")
  }
  call<-merge(c11,c21,by = "����",all = TRUE)
  call<-merge(call,c31,by = "����",all = TRUE)
  call<-merge(call,c41,by = "����",all = TRUE)
  final<-merge(c,call,by = "����",all.x = TRUE)
  final<-merge(final,b1,by = "����",all = TRUE)
  write.csv(final,paste("��������֤�ջ����",aa,".csv",sep=""))
}