#ͨ�ò�����
dhspc<-function(ldate,dhmode=c(1:2))
{a<-loadN()
b<-read.csv(paste("lab/dh/",as.character(Sys.Date()),".csv",sep=""))
��ʼ����<-as.Date(b$��ʼʱ��)
b<-cbind(b,��ʼ����)
mdate<-ldate


#max(b$��ʼ����)
c<-subset(b,��ʼ����==mdate)

a$��˿�ʼʱ��[a$��˿�ʼʱ��==""]<-"1900-1-3 0:00"
a$��˿�ʼʱ��<-as.Date(a$��˿�ʼʱ��)
a<-subset(a,��˿�ʼʱ��>as.Date("2010-01-01"))
a2<-a[c("������","��������","��Ʒ����","��������","��������","���˵�˵绰",
        "��λ��˵绰����","��λ��˵绰����","��λ��˵绰�ֻ���")]

write.csv(a2,"temp/qq.csv",row.names = F)
a<-read.csv("temp/qq.csv")

##--------���粿��--------
a$���˵�˵绰<-gsub("=\"","",a$���˵�˵绰)
a$���˵�˵绰<-gsub("\"","",a$���˵�˵绰)
a$��λ��˵绰����<-gsub("=\"","",a$��λ��˵绰����)
a$��λ��˵绰����<-gsub("\"","",a$��λ��˵绰����)
a$��λ��˵绰����<-gsub("=\"","",a$��λ��˵绰����)
a$��λ��˵绰����<-gsub("\"","",a$��λ��˵绰����)
a$��λ��˵绰�ֻ���<-gsub("=\"","",a$��λ��˵绰�ֻ���)
a$��λ��˵绰�ֻ���<-gsub("\"","",a$��λ��˵绰�ֻ���)
a$��λ��˵绰����<-as.character(as.numeric(a$��λ��˵绰����))
a$��λ��˵绰����[a$��λ��˵绰����=='0512']<-""
a$��λ��˵绰����[a$��λ��˵绰����=='512']<-""
a$��λ��˵绰����<-gsub(" ","",a$��λ��˵绰����)
a$��λ��˵绰����<-gsub(" ","",a$��λ��˵绰����)
a$��λ��˵绰�ֻ���<-gsub(" ","",a$��λ��˵绰�ֻ���)
��λ��˵绰1<-paste(a$��λ��˵绰����,a$��λ��˵绰����,sep="")
��λ��˵绰2<-paste(a$��λ��˵绰����,a$��λ��˵绰����,a$��λ��˵绰�ֻ���,sep="")
a<-a[,-(ncol(a)-2):-ncol(a)]
a<-cbind(a,��λ��˵绰1,��λ��˵绰2)
abase<-a

##���粿��##
ax<-ddply(a,.(���˵�˵绰),summarize,��������=max(as.Date(��������)))
�ж�1<-paste(ax$���˵�˵绰,"-",as.numeric(as.Date(ax$��������)),sep="")
ax<-cbind(ax,�ж�1)
�ж�1<-paste(a$���˵�˵绰,"-",as.numeric(as.Date(a$��������)),sep="")
a<-cbind(a,�ж�1)
q<-merge(a,ax,by="�ж�1",all.x = T,all.y = F)
q2<-q[is.na(q$���˵�˵绰.y)==F,]
# ����q2!=ax����Ϊͬһ��ͬһ�ͻ��ظ��������

#############################ԭ���任 ע���޳��е�����
colnames(q2)[which(colnames(q2)=="���˵�˵绰.x")]<-"���˵�˵绰"
a<-q2
# vlistΪƥ�������һ�����յ��������ϸ
vlist<-merge(c,a,by.x = "���к���",by.y="���˵�˵绰",all.x = T,all.y = F)
write.csv(vlist,"temp/vlist.csv")
vlist<-read.csv("temp/vlist.csv")



vlist<-subset(vlist,���з�ʽ=="����")

####��ʱ���� ���� 9191����¼(������ƥ���е�)####
vlistp2<-ddply(vlist,.(���к���),summarize,������ʱ��=max(as.POSIXct(��ʼʱ��)))
�ж�x<-paste(vlistp2$���к���,as.numeric(vlistp2$������ʱ��),sep="-")
vlistp2<-cbind(vlistp2,�ж�x)
�ж�y<-paste(vlist$���к���,as.numeric(as.POSIXct(vlist$��ʼʱ��)),sep="-")
vlist<-cbind(vlist,�ж�y)
qx<-merge(vlist,vlistp2,by.x="�ж�y",by.y="�ж�x",all.x = T,all.y = F)
qx<-qx[is.na(qx$������ʱ��)==F,]
vlist<-qx
vlist<-subset(vlist,���н��=="����")
vr<-which(colnames(vlist)=="���к���.x")
colnames(vlist)[vr]<-"���к���"


vlist2<-ddply(vlist,.(���к���,��Ʒ����,��������),summarize,��ʼʱ��=max(as.POSIXct(��ʼʱ��)))
vlist2<-arrange(vlist2,��ʼʱ��)

��ʼʱ��2<-as.numeric(vlist2$��ʼʱ��)
�����<-paste(vlist2$���к���,"-",��ʼʱ��2,sep="")
vlist2<-cbind(vlist2,�����)

��ʼʱ��3<-as.numeric(as.POSIXct(vlist$��ʼʱ��))
�����2<-paste(vlist$���к���,"-",��ʼʱ��3,sep="")
vlist<-cbind(vlist,�����2)

vlist2<-vlist2[c("�����","��Ʒ����")]
vlist3<-merge(vlist2,vlist,by.x="�����",by.y="�����2",all.x=T,all.y=F)
vlist3<-arrange(vlist3,��ʼʱ��)
vlist3<-vlist3[c("������","ͨ��ʱ��","��������","��Ʒ����.x","��������","��������",
                 "���к���","��ϯ���","��ʼʱ��","����ʱ��")]
colnames(vlist3)[which(colnames(vlist3)=="��Ʒ����.x")]<-"��Ʒ����"

vlist3<-subset(vlist3,��Ʒ����=="B+C���ô���" | ��Ʒ����=="���״����ô���" |��Ʒ����=="B+���״�����Ӫ��")

vlist3$ͨ��ʱ��<-as.character(vlist3$ͨ��ʱ��)
ͨ��ʱ��x<-as.numeric(substr(vlist3$ͨ��ʱ��,7,8))+60*(as.numeric(substr(vlist3$ͨ��ʱ��,4,5)))+3600*(as.numeric(substr(vlist3$ͨ��ʱ��,1,2)))
vlist3<-cbind(vlist3,ͨ��ʱ��x)

vlist3<-vlist3[c("������","ͨ��ʱ��x","��������","��Ʒ����","��������","��������",
                 "���к���","��ϯ���","��ʼʱ��","����ʱ��","ͨ��ʱ��")]

write.csv(vlist3,"temp/vlist.csv")
vlist3<-read.csv("temp/vlist.csv")

qq<-read.csv(paste("lab/dh2/",as.character(Sys.Date()),".csv",sep=""))
qq<-subset(qq,�������=="��˸���")
qq<-subset(qq,��˽��=="ͨ��")
qq<-qq[as.Date(qq$��˽���ʱ��)<=as.Date(ldate),]
qq2<-merge(vlist3,qq,by="������",all.x = T,all.y = F)
qq2<-qq2[is.na(qq2$��˽��)==F,]

qq2<-qq2[c("������","ͨ��ʱ��x","��������.x","��Ʒ����.x","��������.x","��������.x",
           "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
           "ͨ��ʱ��")]

colnames(qq2)<-c("������","ͨ��ʱ��s","��������","��Ʒ����","��������","��������",
                 "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
                 "ͨ��ʱ��")

qq2p0<-qq2

##���粿��p1##
a<-abase
ax<-ddply(a,.(��λ��˵绰1),summarize,��������=max(as.Date(��������)))
�ж�1<-paste(ax$��λ��˵绰1,"-",as.numeric(as.Date(ax$��������)),sep="")
ax<-cbind(ax,�ж�1)
�ж�1<-paste(a$��λ��˵绰1,"-",as.numeric(as.Date(a$��������)),sep="")
a<-cbind(a,�ж�1)
q<-merge(a,ax,by="�ж�1",all.x = T,all.y = F)
q2<-q[is.na(q$��λ��˵绰1.y)==F,]
q2<-q2[q2$��λ��˵绰1.y!="",]
#############################ԭ���任 ע���޳��е�����
colnames(q2)[which(colnames(q2)=="��λ��˵绰1.x")]<-"��λ��˵绰"
a<-q2

vlist<-merge(c,a,by.x = "���к���",by.y="��λ��˵绰",all.x = T,all.y = F)
write.csv(vlist,"temp/vlist.csv")
vlist<-read.csv("temp/vlist.csv")


vlist<-subset(vlist,���з�ʽ=="����")

####��ʱ���� ���� 9191����¼(������ƥ���е�)####
vlistp2<-ddply(vlist,.(���к���),summarize,������ʱ��=max(as.POSIXct(��ʼʱ��)))
�ж�x<-paste(vlistp2$���к���,as.numeric(vlistp2$������ʱ��),sep="-")
vlistp2<-cbind(vlistp2,�ж�x)
�ж�y<-paste(vlist$���к���,as.numeric(as.POSIXct(vlist$��ʼʱ��)),sep="-")
vlist<-cbind(vlist,�ж�y)
qx<-merge(vlist,vlistp2,by.x="�ж�y",by.y="�ж�x",all.x = T,all.y = F)
qx<-qx[is.na(qx$������ʱ��)==F,]
vlist<-qx
vlist<-subset(vlist,���н��=="����")
vr<-which(colnames(vlist)=="���к���.x")
colnames(vlist)[vr]<-"���к���"

vlist2<-ddply(vlist,.(���к���,��Ʒ����,��������),summarize,��ʼʱ��=max(as.POSIXct(��ʼʱ��)))
vlist2<-arrange(vlist2,��ʼʱ��)

��ʼʱ��2<-as.numeric(vlist2$��ʼʱ��)
�����<-paste(vlist2$���к���,"-",��ʼʱ��2,sep="")
vlist2<-cbind(vlist2,�����)

��ʼʱ��3<-as.numeric(as.POSIXct(vlist$��ʼʱ��))
�����2<-paste(vlist$���к���,"-",��ʼʱ��3,sep="")
vlist<-cbind(vlist,�����2)

vlist2<-vlist2[c("�����","��Ʒ����")]
vlist3<-merge(vlist2,vlist,by.x="�����",by.y="�����2",all.x=T,all.y=F)
vlist3<-arrange(vlist3,��ʼʱ��)
vlist3<-vlist3[c("������","ͨ��ʱ��","��������","��Ʒ����.x","��������","��������",
                 "���к���","��ϯ���","��ʼʱ��","����ʱ��")]
colnames(vlist3)[which(colnames(vlist3)=="��Ʒ����.x")]<-"��Ʒ����"

vlist3<-subset(vlist3,��Ʒ����=="B+C���ô���" | ��Ʒ����=="���״����ô���" |��Ʒ����=="B+���״�����Ӫ��")

vlist3$ͨ��ʱ��<-as.character(vlist3$ͨ��ʱ��)
ͨ��ʱ��x<-as.numeric(substr(vlist3$ͨ��ʱ��,7,8))+60*(as.numeric(substr(vlist3$ͨ��ʱ��,4,5)))+3600*(as.numeric(substr(vlist3$ͨ��ʱ��,1,2)))

vlist3<-cbind(vlist3,ͨ��ʱ��x)

vlist3<-vlist3[c("������","ͨ��ʱ��x","��������","��Ʒ����","��������","��������",
                 "���к���","��ϯ���","��ʼʱ��","����ʱ��","ͨ��ʱ��")]

write.csv(vlist3,"temp/vlist.csv")
vlist3<-read.csv("temp/vlist.csv")

qq<-read.csv(paste("lab/dh2/",as.character(Sys.Date()),".csv",sep=""))
qq<-subset(qq,�������=="��˵�λ")
qq<-subset(qq,��˽��=="ͨ��")
qq<-qq[as.Date(qq$��˽���ʱ��)<=as.Date(ldate),]
qq2<-merge(vlist3,qq,by="������",all.x = T,all.y = F)
qq2<-qq2[is.na(qq2$��˽��)==F,]

qq2<-qq2[c("������","ͨ��ʱ��x","��������.x","��Ʒ����.x","��������.x","��������.x",
           "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
           "ͨ��ʱ��")]

colnames(qq2)<-c("������","ͨ��ʱ��s","��������","��Ʒ����","��������","��������",
                 "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
                 "ͨ��ʱ��")

qq2p1<-qq2
##δ��/ƴ�����½��

##���粿��p2##
a<-abase
ax<-ddply(a,.(��λ��˵绰2),summarize,��������=max(as.Date(��������)))
�ж�1<-paste(ax$��λ��˵绰2,"-",as.numeric(as.Date(ax$��������)),sep="")
ax<-cbind(ax,�ж�1)
�ж�1<-paste(a$��λ��˵绰2,"-",as.numeric(as.Date(a$��������)),sep="")
a<-cbind(a,�ж�1)
q<-merge(a,ax,by="�ж�1",all.x = T,all.y = F)
q2<-q[is.na(q$��λ��˵绰2.y)==F,]
q2<-q2[q2$��λ��˵绰2.y!="",]
####ԭ���任 ע���޳��е���####
colnames(q2)[which(colnames(q2)=="��λ��˵绰2.x")]<-"��λ��˵绰"
a<-q2

vlist<-merge(c,a,by.x = "���к���",by.y="��λ��˵绰",all.x = T,all.y = F)
write.csv(vlist,"temp/vlist.csv")
vlist<-read.csv("temp/vlist.csv")

vlist<-subset(vlist,���з�ʽ=="����")

####��ʱ���� ���� 9191����¼(������ƥ���е�)####
vlistp2<-ddply(vlist,.(���к���),summarize,������ʱ��=max(as.POSIXct(��ʼʱ��)))
�ж�x<-paste(vlistp2$���к���,as.numeric(vlistp2$������ʱ��),sep="-")
vlistp2<-cbind(vlistp2,�ж�x)
�ж�y<-paste(vlist$���к���,as.numeric(as.POSIXct(vlist$��ʼʱ��)),sep="-")
vlist<-cbind(vlist,�ж�y)
qx<-merge(vlist,vlistp2,by.x="�ж�y",by.y="�ж�x",all.x = T,all.y = F)
qx<-qx[is.na(qx$������ʱ��)==F,]
vlist<-qx
vlist<-subset(vlist,���н��=="����")
vr<-which(colnames(vlist)=="���к���.x")
colnames(vlist)[vr]<-"���к���"
vlist2<-ddply(vlist,.(���к���,��Ʒ����,��������),summarize,��ʼʱ��=max(as.POSIXct(��ʼʱ��)))
vlist2<-arrange(vlist2,��ʼʱ��)

��ʼʱ��2<-as.numeric(vlist2$��ʼʱ��)
�����<-paste(vlist2$���к���,"-",��ʼʱ��2,sep="")
vlist2<-cbind(vlist2,�����)

��ʼʱ��3<-as.numeric(as.POSIXct(vlist$��ʼʱ��))
�����2<-paste(vlist$���к���,"-",��ʼʱ��3,sep="")
vlist<-cbind(vlist,�����2)

vlist2<-vlist2[c("�����","��Ʒ����")]
vlist3<-merge(vlist2,vlist,by.x="�����",by.y="�����2",all.x=T,all.y=F)
vlist3<-arrange(vlist3,��ʼʱ��)
vlist3<-vlist3[c("������","ͨ��ʱ��","��������","��Ʒ����.x","��������","��������",
                 "���к���","��ϯ���","��ʼʱ��","����ʱ��")]
colnames(vlist3)[which(colnames(vlist3)=="��Ʒ����.x")]<-"��Ʒ����"

vlist3<-subset(vlist3,��Ʒ����=="B+C���ô���" | ��Ʒ����=="���״����ô���" |��Ʒ����=="B+���״�����Ӫ��")

vlist3$ͨ��ʱ��<-as.character(vlist3$ͨ��ʱ��)
ͨ��ʱ��x<-as.numeric(substr(vlist3$ͨ��ʱ��,7,8))+60*(as.numeric(substr(vlist3$ͨ��ʱ��,4,5)))+3600*(as.numeric(substr(vlist3$ͨ��ʱ��,1,2)))

vlist3<-cbind(vlist3,ͨ��ʱ��x)

vlist3<-vlist3[c("������","ͨ��ʱ��x","��������","��Ʒ����","��������","��������",
                 "���к���","��ϯ���","��ʼʱ��","����ʱ��","ͨ��ʱ��")]

write.csv(vlist3,"temp/vlist.csv")
vlist3<-read.csv("temp/vlist.csv")

qq<-read.csv(paste("lab/dh2/",as.character(Sys.Date()),".csv",sep=""))
qq<-subset(qq,�������=="��˵�λ")
qq<-subset(qq,��˽��=="ͨ��")
qq<-qq[as.Date(qq$��˽���ʱ��)<=as.Date(ldate),]
qq2<-merge(vlist3,qq,by="������",all.x = T,all.y = F)
qq2<-qq2[is.na(qq2$��˽��)==F,]

qq2<-qq2[c("������","ͨ��ʱ��x","��������.x","��Ʒ����.x","��������.x","��������.x",
           "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
           "ͨ��ʱ��")]

colnames(qq2)<-c("������","ͨ��ʱ��s","��������","��Ʒ����","��������","��������",
                 "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
                 "ͨ��ʱ��")

qq2p2<-qq2
###�ϲ�2/3���
q2f<-rbind(qq2p1,qq2p2)
q2f1<-unique(q2f)
q2f0<-qq2p0

#######����q2f0 ����q2f1#####
std<-read.csv("lab/��˺�ʱ��׼.csv")
#�Ȳ��ֲ�Ʒ
idvd_l<-std$����[3]
idvd_h<-std$����[3]
wp_l<-std$����[4]
wp_h<-std$����[4]

q2f1<-arrange(q2f1,��ʼʱ��)
q2f0<-arrange(q2f0,��ʼʱ��)

q2f1$ͨ��ʱ��s<-as.numeric(q2f1$ͨ��ʱ��s)
q2f0$ͨ��ʱ��s<-as.numeric(q2f0$ͨ��ʱ��s)

�����<-subset(q2f0,ͨ��ʱ��s<idvd_l)
�����<-subset(q2f0,ͨ��ʱ��s>idvd_h)
��������<-subset(q2f0,ͨ��ʱ��s>=idvd_l & ͨ��ʱ��s<=idvd_h)

�����<-subset(q2f1,ͨ��ʱ��s<wp_l)
�����<-subset(q2f1,ͨ��ʱ��s>wp_h)
��������<-subset(q2f1,ͨ��ʱ��s>=wp_l & ͨ��ʱ��s<=wp_h)


###�����ļ���
dir.create(paste(as.character(Sys.Date())," ���ʱЧ���",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," ���ʱЧ���/","���״�-����",sep = ""), showWarnings = FALSE)
dir.create(paste(as.character(Sys.Date())," ���ʱЧ���/","���״�-����",sep = ""), showWarnings = FALSE)

if ( 1 %in% dhmode)
  
{spc_imrdh(��������,dir="���״�-����")}

if ( 2 %in% dhmode)
{spc_imrdh(��������,dir="���״�-����")}

�����<-�����[c("������","��������","��Ʒ����","ͨ��ʱ��s","��������","��������",
           "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
           "ͨ��ʱ��")]

�����<-�����[c("������","��������","��Ʒ����","ͨ��ʱ��s","��������","��������",
           "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
           "ͨ��ʱ��")]

��������<-��������[c("������","��������","��Ʒ����","ͨ��ʱ��s","��������","��������",
             "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
             "ͨ��ʱ��")]

�����<-�����[c("������","��������","��Ʒ����","ͨ��ʱ��s","��������","��������",
           "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
           "ͨ��ʱ��")]

�����<-�����[c("������","��������","��Ʒ����","ͨ��ʱ��s","��������","��������",
           "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
           "ͨ��ʱ��")]

��������<-��������[c("������","��������","��Ʒ����","ͨ��ʱ��s","��������","��������",
             "��Ⱥ���","����״̬","��ǰ���̲���","���к���","��ϯ���","��ʼʱ��","����ʱ��",
             "ͨ��ʱ��")]

##���
write.csv(�����,paste(as.character(Sys.Date())," ���ʱЧ���","/���״�-����/1.ʱЧ�����.csv",sep = ""))
write.csv(�����,paste(as.character(Sys.Date())," ���ʱЧ���","/���״�-����/2.����ʱЧ��.csv",sep = ""))
write.csv(��������,paste(as.character(Sys.Date())," ���ʱЧ���","/���״�-����/3.ʱЧ������.csv",sep = ""))

write.csv(�����,paste(as.character(Sys.Date())," ���ʱЧ���","/���״�-����/1.ʱЧ�����.csv",sep = ""))
write.csv(�����,paste(as.character(Sys.Date())," ���ʱЧ���","/���״�-����/2.����ʱЧ��.csv",sep = ""))
write.csv(��������,paste(as.character(Sys.Date())," ���ʱЧ���","/���״�-����/3.ʱЧ������.csv",sep = ""))}