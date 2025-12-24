# SFU3240V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3240V.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＡＣＯＳ振替改善　　　　　　　　　*
*    モジュール名　　　　：　振替更新データリストＣＳＶ出力　　*
*    作成日／更新日　　　：　2017/01/06                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタを受取り、振替データ累積  *
*                        ：　ファイルより振替ＤＴのＣＳＶ出力  *
*                        ：　を行う。　　　　　　　　　　　　  *
*    更新日／更新者　　　：　2017/03/30 NAV TAKAHASHI          *
*    更新概要　　　　　　：　ストック_項目追加　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SFU3240V.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2016/01/06.
****************************************************************
 ENVIRONMENT               DIVISION.
****************************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
*    STATION     IS        STA
     YA          IS        YA
     YB          IS        YB
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
****************************************************************
 INPUT-OUTPUT           SECTION.
****************************************************************
 FILE-CONTROL.
*----<< ACOS振替データ累積ファイル >>--*
     SELECT     FRACOSF      ASSIGN    TO        DA-01-VI-FRACOSL2
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      SEQUENTIAL
                             RECORD    KEY       FRA-F01
                                                 FRA-F03
                                                 FRA-F04
                                                 FRA-F08
                                                 FRA-F09
                                                 FRA-F05
                                                 FRA-F06
                                                 FRA-F07
                             FILE      STATUS    FRA-ST.
*----<< 倉庫マスタ >>--*                                                
     SELECT     ZSOKMS       ASSIGN    TO       DA-01-VI-ZSOKMS1          
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      SOK-F01                   
                             FILE      STATUS   SOK-ST.                   
*----<< 部門マスタ >>--*                                                
     SELECT     BUMONF       ASSIGN    TO       DA-01-VI-BUMONL1          
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      BUM-F01                   
                             FILE      STATUS   BUM-ST.                   
*----<< 条件ファイル >>--*                                              
     SELECT     HJYOKEN      ASSIGN    TO       DA-01-VI-JYOKEN1          
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      JYO-F01  JYO-F02          
                             FILE      STATUS   JYO-ST.                   
*----<< 仕入先マスタ >>--*                                              
     SELECT     ZSHIMS       ASSIGN    TO       DA-01-VI-ZSHIMS1          
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      SHI-F01                   
                             FILE      STATUS   SHI-ST.                   
*----<< 取引先マスタ >>--*                                              
     SELECT     HTOKMS       ASSIGN    TO       DA-01-VI-TOKMS2           
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      TOK-F01                   
                             FILE      STATUS   TOK-ST.                   
*----<< 商品名称マスタ >>--*                                            
     SELECT     HMEIMS       ASSIGN    TO       DA-01-VI-MEIMS1           
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      MEI-F011                  
                                                MEI-F0121                 
                                                MEI-F0122                 
                                                MEI-F0123                 
                             FILE      STATUS   MEI-ST.                   
*----<< 振替更新データＣＳＶ出力Ｆ >>--*
     SELECT     FURIKLST     ASSIGN    TO       DA-01-FURIKLST
                             ORGANIZATION       SEQUENTIAL                
                             ACCESS    MODE     SEQUENTIAL                
                             FILE      STATUS   LST-ST.                   
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
*----<< ACOS振替データ累積ファイル >>--*
 FD  FRACOSF     LABEL       RECORD    IS        STANDARD.
     COPY        FRACOSF   OF        XFDLIB
     JOINING     FRA       AS        PREFIX.
*----<< 倉庫マスタ >>--*                                                
 FD  ZSOKMS.                                                              
     COPY        ZSOKMS      OF        XFDLIB                             
     JOINING     SOK         AS        PREFIX.                            
*----<< 部門マスタ >>--*                                                
 FD  BUMONF.                                                              
     COPY        BUMONF      OF        XFDLIB                             
     JOINING     BUM         AS        PREFIX.                            
*----<< 条件ファイル >>--*                                              
 FD  HJYOKEN.                                                             
     COPY        HJYOKEN     OF        XFDLIB                             
     JOINING     JYO         AS        PREFIX.                            
*----<< 仕入先マスタ >>--*                                              
 FD  ZSHIMS.                                                              
     COPY        ZSHIMS      OF        XFDLIB                             
     JOINING     SHI         AS        PREFIX.                            
*----<< 取引先マスタ >>--*                                              
 FD  HTOKMS.                                                              
     COPY        HTOKMS      OF        XFDLIB                             
     JOINING     TOK         AS        PREFIX.                            
*----<< 商品名称マスタ >>--*                                            
 FD  HMEIMS.                                                              
     COPY        HMEIMS      OF        XFDLIB                             
     JOINING     MEI         AS        PREFIX.                            
*----<< 振替更新データＣＳＶ出力Ｆ >>--*
 FD  FURIKLST            BLOCK    CONTAINS   1         RECORDS.           
*                                                                         
 01  LST-REC                 PIC  X(1000).
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
*
 01  FILE-STATUS.
     03  FRA-ST              PIC  X(02).
     03  SOK-ST              PIC  X(02).
     03  BUM-ST              PIC  X(02).
     03  JYO-ST              PIC  X(02).
     03  SHI-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  SYO-ST              PIC  X(02).
     03  MEI-ST              PIC  X(02).
     03  LST-ST              PIC  X(02).
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE   SPACE.
     03  ZSOKMS-INV-FLG      PIC  X(03)  VALUE   SPACE.
     03  BUMONF-INV-FLG      PIC  X(03)  VALUE   SPACE.
     03  HJYOKEN-INV-FLG     PIC  X(03)  VALUE   SPACE.
     03  ZSHIMS-INV-FLG      PIC  X(03)  VALUE   SPACE.
     03  HTOKMS-INV-FLG      PIC  X(03)  VALUE   SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE   SPACE.
     03  PAGE-CNT            PIC  9(05)  VALUE   ZERO.
     03  LINE-CNT            PIC  9(05)  VALUE   ZERO.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE            PIC  9(08).
*
 01  WK-SURYO                PIC  9(07)V9(02).
 01  WK-SURYO-R        REDEFINES      WK-SURYO.
     03  WK-SURYO1           PIC  9(07).
     03  WK-SURYO2           PIC  9(02).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
*
 01  FILE-ERR.
     03  FRA-ERR             PIC  N(15) VALUE
         NC"ＡＣＯＳ振替データ累積Ｆエラー".
     03  SOK-ERR             PIC  N(15) VALUE
         NC"倉庫マスタエラー".
     03  BUM-ERR             PIC  N(15) VALUE
         NC"部門マスタエラー".
     03  JYO-ERR             PIC  N(15) VALUE
         NC"条件ファイルエラー".
     03  SHI-ERR             PIC  N(15) VALUE
         NC"仕入先マスタエラー".
     03  TOK-ERR             PIC  N(15) VALUE
         NC"取引先マスタエラー".
     03  SYO-ERR             PIC  N(15) VALUE
         NC"商品名称マスタエラー".
     03  LST-ERR             PIC  N(15) VALUE
         NC"振替更新データＣＳＶエラー".
*
 01  READ-CNT                PIC  9(07) VALUE  0.
 01  TAIS-CNT                PIC  9(07) VALUE  0.
*
 01  NEW-KEY.
     02  NEW-JYUSIN          PIC  9(08).
     02  NEW-SOKCD           PIC  X(02).
*
 01  OLD-KEY.
     02  OLD-JYUSIN          PIC  9(08).
     02  OLD-SOKCD           PIC  X(02).
*
****　ヘッダ１
 01  HEAD01.
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(12)  VALUE
         NC"＜振替更新データリスト＞".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD01-01             PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD01-02             PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  HD01-DTKBN          PIC  N(07).
     02  FILLER              PIC  X(01)  VALUE  X"29".
****  ヘッダ２
 01  HEAD02.
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"受信日開始".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"受信日終了".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(04)  VALUE
         NC"倉庫開始".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(04)  VALUE
         NC"倉庫終了".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"出荷日開始".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"出荷日終了".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(06)  VALUE
         NC"商品ＣＤ開始".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(06)  VALUE
         NC"商品ＣＤ終了".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"伝票区分１".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"伝票区分２".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"伝票区分３".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"伝票区分４".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"伝票区分５".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"伝票区分６".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"伝票区分７".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"伝票区分８".
     02  FILLER              PIC  X(01)  VALUE  X"29".
****  ヘッダ３
 01  HEAD03.
     02  HD03-01             PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-02             PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-03             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-04             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-05             PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-06             PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-07             PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-08             PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-09             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-10             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-11             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-12             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-13             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-14             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-15             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  HD03-16             PIC  X(02).
****  ヘッダ４
 01  HEAD04.
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(03)  VALUE
         NC"受信日".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(04)  VALUE
         NC"倉庫ＣＤ".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(03)  VALUE
         NC"倉庫名".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(04)  VALUE
         NC"伝票区分".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(03)  VALUE
         NC"出荷日".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(02)  VALUE
         NC"部門".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"振替情報１".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"振替情報２".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(04)  VALUE
         NC"商品ＣＤ".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(04)  VALUE
         NC"品単ＣＤ".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(03)  VALUE
         NC"商品名".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(05)  VALUE
         NC"入出庫区分".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(02)  VALUE
         NC"数量".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(06)  VALUE
         NC"ストックＮＯ".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(02)  VALUE
         NC"単価".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(08)  VALUE
         NC"伝票番号＋行番号".
     02  FILLER              PIC  X(01)  VALUE  X"29".
     02  FILLER              PIC  X(01)  VALUE  ",".
     02  FILLER              PIC  X(01)  VALUE  X"28".
     02  FILLER              PIC  N(01)  VALUE
         NC"更".
     02  FILLER              PIC  X(01)  VALUE  X"29".
****  明細
 01  MEISAI01.
     02  MS01-01             PIC  X(10).
     02  MS01-01K            PIC  X(01).
     02  MS01-02             PIC  X(02).
     02  MS01-02K            PIC  X(01).
     02  MS01-03S            PIC  X(01).
     02  MS01-03             PIC  N(10).
     02  MS01-03E            PIC  X(01).
     02  MS01-03K            PIC  X(01).
     02  MS01-04             PIC  9(02).
     02  MS01-041            PIC  X(01).
     02  MS01-04S            PIC  X(01).
     02  MS01-04N            PIC  N(05).
     02  MS01-04E            PIC  X(01).
     02  MS01-04K            PIC  X(01).
     02  MS01-05             PIC  X(10).
     02  MS01-05K            PIC  X(01).
     02  MS01-06             PIC  X(04).
     02  MS01-06K            PIC  X(01).
     02  MS01-07             PIC  X(08).
     02  MS01-07K            PIC  X(01).
     02  MS01-08S            PIC  X(01).
     02  MS01-08             PIC  N(10).
     02  MS01-08E            PIC  X(01).
     02  MS01-08K            PIC  X(01).
     02  MS01-09             PIC  X(08).
     02  MS01-09K            PIC  X(01).
     02  MS01-10             PIC  X(08).
     02  MS01-10K            PIC  X(01).
     02  MS01-11S            PIC  X(01).
     02  MS01-11             PIC  N(30).
     02  MS01-11E            PIC  X(01).
     02  MS01-11K            PIC  X(01).
     02  MS01-12S            PIC  X(01).
     02  MS01-12             PIC  N(10).
     02  MS01-12E            PIC  X(01).
     02  MS01-12K            PIC  X(01).
     02  MS01-131            PIC  X(01).
     02  MS01-13             PIC  9(09).
     02  MS01-132            PIC  X(01).
     02  MS01-133            PIC  9(02).
     02  MS01-13K            PIC  X(01).
     02  MS01-14             PIC  X(06).
     02  MS01-14K            PIC  X(01).
     02  MS01-15             PIC  9(09).
     02  MS01-152            PIC  X(01).
     02  MS01-153            PIC  9(02).
     02  MS01-15K            PIC  X(01).
     02  MS01-16             PIC  X(12).
     02  MS01-16K            PIC  X(01).
     02  MS01-17S            PIC  X(01).
     02  MS01-17             PIC  N(05).
     02  MS01-17E            PIC  X(01).
*
 LINKAGE                   SECTION.
 01  PARA-JDTST              PIC  9(08).
 01  PARA-JDTED              PIC  9(08).
 01  PARA-SOKST              PIC  X(02).
 01  PARA-SOKED              PIC  X(02).
 01  PARA-SKDST              PIC  9(08).
 01  PARA-SKDED              PIC  9(08).
 01  PARA-SYOST              PIC  X(08).
 01  PARA-SYOED              PIC  X(08).
 01  PARA-DENK1              PIC  X(02).
 01  PARA-DENK2              PIC  X(02).
 01  PARA-DENK3              PIC  X(02).
 01  PARA-DENK4              PIC  X(02).
 01  PARA-DENK5              PIC  X(02).
 01  PARA-DENK6              PIC  X(02).
 01  PARA-DENK7              PIC  X(02).
 01  PARA-DENK8              PIC  X(02).
 01  PARA-DTKBN              PIC  X(01).
*
****************************************************************
 PROCEDURE                 DIVISION  USING  PARA-JDTST
                                            PARA-JDTED
                                            PARA-SOKST
                                            PARA-SOKED
                                            PARA-SKDST
                                            PARA-SKDED
                                            PARA-SYOST
                                            PARA-SYOED
                                            PARA-DENK1
                                            PARA-DENK2
                                            PARA-DENK3
                                            PARA-DENK4
                                            PARA-DENK5
                                            PARA-DENK6
                                            PARA-DENK7
                                            PARA-DENK8
                                            PARA-DTKBN.
****************************************************************
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE FURIKLST.
     DISPLAY     LST-ERR   UPON      CONS.
     DISPLAY     LST-ST    UPON      CONS.
     STOP        RUN.
 FRA-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE FRACOSF.
     DISPLAY     FRA-ERR   UPON      CONS.
     DISPLAY     FRA-ST    UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 BUM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE BUMONF.
     DISPLAY     BUM-ERR   UPON      CONS.
     DISPLAY     BUM-ST    UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SHI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSHIMS.
     DISPLAY     SHI-ERR   UPON      CONS.
     DISPLAY     SHI-ST    UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     SYO-ERR   UPON      CONS.
     DISPLAY     SYO-ST    UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
*
     PERFORM     MAIN-SEC   UNTIL  END-FLG = "END".
*
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ
**********************************************************
 INIT-SEC                  SECTION.
*
*    DISPLAY "PARA-JDTST = " PARA-JDTST UPON CONS.
*    DISPLAY "PARA-JDTED = " PARA-JDTED UPON CONS.
*    DISPLAY "PARA-SOKST = " PARA-SOKST UPON CONS.
*    DISPLAY "PARA-SOKED = " PARA-SOKED UPON CONS.
*    DISPLAY "PARA-SKDST = " PARA-SKDST UPON CONS.
*    DISPLAY "PARA-SKDED = " PARA-SKDED UPON CONS.
*    DISPLAY "PARA-SYOST = " PARA-SYOST UPON CONS.
*    DISPLAY "PARA-SYOED = " PARA-SYOED UPON CONS.
*----<< ｼｽﾃﾑﾋﾂﾞｹ ｼｭﾄｸ >>--*
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE      20                 TO   WK-YS.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*
*----<< ｼｽﾃﾑｼﾞｺｸ ｼｭﾄｸ >>--*
     ACCEPT    WK-TIME          FROM   TIME.
*
*----<<FILE OPEN >>--*
     OPEN        INPUT     FRACOSF  ZSOKMS  BUMONF  HJYOKEN
                           ZSHIMS   HTOKMS  HMEIMS
                 OUTPUT    FURIKLST.
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     INITIALIZE                        NEW-KEY  OLD-KEY.
*ＡＣＯＳ振替データ累積ファイルスタート
     MOVE      SPACE              TO   FRA-REC.
     INITIALIZE                        FRA-REC
     MOVE      PARA-JDTST         TO   FRA-F01.
     MOVE      PARA-SOKST         TO   FRA-F03.
     START  FRACOSF  KEY  IS  >=  FRA-F01  FRA-F03  FRA-F04
                                  FRA-F08  FRA-F09  FRA-F05
                                  FRA-F06  FRA-F07
            INVALID
            MOVE  "END"           TO   END-FLG
            DISPLAY NC"＃＃対象データ無（ＳＴ）＃＃" UPON CONS
            GO                    TO   INIT-EXIT
     END-START.
*
     DISPLAY "PARA-SOKST=" PARA-SOKST " PARA-SOKED=" PARA-SOKED
              UPON CONS.
     PERFORM  FRACOSF-READ-SEC.
     IF  END-FLG  =  "END"
         DISPLAY NC"＃＃対象データ無（ＲＤ）＃＃" UPON CONS
         GO                       TO   INIT-EXIT
     END-IF.
*ヘッダ行出力
     PERFORM  HEAD-WRT-SEC.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
 FRACOSF-READ-SEC          SECTION.
*
 READ010.
     READ  FRACOSF
           AT  END    MOVE  "END"   TO   END-FLG
                      GO            TO   FRACOSF-READ-EXIT
           NOT  AT  END
                      ADD    1      TO   READ-CNT
     END-READ.
 READ020.
*読込カウント表示
     IF  READ-CNT(5:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
 READ030.
*受信日範囲チェック
     IF  FRA-F01  >=  PARA-JDTST
     AND FRA-F01  <=  PARA-JDTED
         CONTINUE
     ELSE
         MOVE   "END"      TO   END-FLG
         GO                TO   FRACOSF-READ-EXIT
     END-IF.
 READ040.
*倉庫ＣＤ範囲チェック
     IF  FRA-F03  >=  PARA-SOKST
     AND FRA-F03  <=  PARA-SOKED
         CONTINUE
     ELSE
         GO                TO   FRACOSF-READ-SEC
     END-IF.
 READ050.
*出荷日範囲チェック
     IF  FRA-F08  >=  PARA-SKDST
     AND FRA-F08  <=  PARA-SKDED
         CONTINUE
     ELSE
         GO                TO   FRACOSF-READ-SEC
     END-IF.
 READ060.
*商品ＣＤ範囲チェック
     IF  FRA-F09  >=  PARA-SYOST
     AND FRA-F09  <=  PARA-SYOED
         CONTINUE
     ELSE
         GO                TO   FRACOSF-READ-SEC
     END-IF.
 READ070
*伝票区分チェック
     IF  PARA-DENK1 = SPACE
     AND PARA-DENK2 = SPACE
     AND PARA-DENK3 = SPACE
     AND PARA-DENK4 = SPACE
     AND PARA-DENK5 = SPACE
     AND PARA-DENK6 = SPACE
     AND PARA-DENK7 = SPACE
     AND PARA-DENK8 = SPACE
         CONTINUE
     ELSE
         IF  FRA-F04 = PARA-DENK1
         OR  FRA-F04 = PARA-DENK2
         OR  FRA-F04 = PARA-DENK3
         OR  FRA-F04 = PARA-DENK4
         OR  FRA-F04 = PARA-DENK5
         OR  FRA-F04 = PARA-DENK6
         OR  FRA-F04 = PARA-DENK7
         OR  FRA-F04 = PARA-DENK8
             CONTINUE
         ELSE
             GO            TO   FRACOSF-READ-SEC
         END-IF
     END-IF.
*出力区分チェック
 READ071.
     IF  PARA-DTKBN NOT = SPACE
         IF   PARA-DTKBN = "1"
              IF  FRA-F10T NOT = "2"
                  GO       TO   FRACOSF-READ-SEC
              END-IF
         ELSE
              IF  PARA-DTKBN = "2"
                  IF  FRA-F10T NOT = "1"
                      GO   TO   FRACOSF-READ-SEC
                  END-IF
              END-IF
         END-IF
     END-IF.
 READ090.
*対象データカウント
     ADD 1                 TO   TAIS-CNT.
*
 FRACOSF-READ-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
 MAIN-SEC                  SECTION.
*明細行セット
     PERFORM  MEIWRT-SEC.
*ＡＣＯＳ振替データ累積ファイル読込
     PERFORM  FRACOSF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ
**********************************************************
 END-SEC                   SECTION.
*----<<FILE CLOSE >>--*
     CLOSE       FRACOSF  ZSOKMS  BUMONF  HJYOKEN
                 ZSHIMS   HTOKMS  HMEIMS
                 FURIKLST.
*
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し
**********************************************************
 HEAD-WRT-SEC                  SECTION.
*
*----<< ﾋﾂﾞｹ ｼﾞｶﾝ >>--*
     MOVE      SYS-DATE(1:4)      TO        HD01-01(1:4).
     MOVE      WK-M               TO        HD01-01(6:2).
     MOVE      WK-D               TO        HD01-01(9:2).
     MOVE      "/"                TO        HD01-01(5:1).
     MOVE      "/"                TO        HD01-01(8:1).
     MOVE      WK-TIME(1:2)       TO        HD01-02(1:2).
     MOVE      ":"                TO        HD01-02(3:1).
     MOVE      WK-TIME(3:2)       TO        HD01-02(4:2).
     MOVE      ":"                TO        HD01-02(6:1).
     MOVE      WK-TIME(5:2)       TO        HD01-02(7:2).
*出力指定条件セット
     MOVE      PARA-JDTST(1:4)    TO        HD03-01(1:4).
     MOVE      PARA-JDTST(5:2)    TO        HD03-01(6:2).
     MOVE      PARA-JDTST(7:2)    TO        HD03-01(9:2).
     MOVE      "/"                TO        HD03-01(5:1).
     MOVE      "/"                TO        HD03-01(8:1).
     MOVE      PARA-JDTED(1:4)    TO        HD03-02(1:4).
     MOVE      PARA-JDTED(5:2)    TO        HD03-02(6:2).
     MOVE      PARA-JDTED(7:2)    TO        HD03-02(9:2).
     MOVE      "/"                TO        HD03-02(5:1).
     MOVE      "/"                TO        HD03-02(8:1).
     MOVE      PARA-SOKST         TO        HD03-03.
     MOVE      PARA-SOKED         TO        HD03-04.
     MOVE      PARA-SKDST(1:4)    TO        HD03-05(1:4).
     MOVE      PARA-SKDST(5:2)    TO        HD03-05(6:2).
     MOVE      PARA-SKDST(7:2)    TO        HD03-05(9:2).
     MOVE      "/"                TO        HD03-05(5:1).
     MOVE      "/"                TO        HD03-05(8:1).
     MOVE      PARA-SKDED(1:4)    TO        HD03-06(1:4).
     MOVE      PARA-SKDED(5:2)    TO        HD03-06(6:2).
     MOVE      PARA-SKDED(7:2)    TO        HD03-06(9:2).
     MOVE      "/"                TO        HD03-06(5:1).
     MOVE      "/"                TO        HD03-06(8:1).
     MOVE      PARA-SYOST         TO        HD03-07.
     MOVE      PARA-SYOED         TO        HD03-08.
     MOVE      PARA-DENK1         TO        HD03-09.
     MOVE      PARA-DENK2         TO        HD03-10.
     MOVE      PARA-DENK3         TO        HD03-11.
     MOVE      PARA-DENK4         TO        HD03-12.
     MOVE      PARA-DENK5         TO        HD03-13.
     MOVE      PARA-DENK6         TO        HD03-14.
     MOVE      PARA-DENK7         TO        HD03-15.
     MOVE      PARA-DENK8         TO        HD03-16.
*出力区分セット
     EVALUATE  PARA-DTKBN
         WHEN  SPACE
         MOVE  NC"（　全　て　）" TO        HD01-DTKBN
         WHEN  "1"
         MOVE  NC"（振替消込分）" TO        HD01-DTKBN
         WHEN  "2"
         MOVE  NC"（エラー分　）" TO        HD01-DTKBN
         WHEN  OTHER
         MOVE  NC"＊＊＊＊＊＊＊" TO        HD01-DTKBN
     END-EVALUATE.
*
     MOVE      SPACE              TO        LST-REC.
     MOVE      HEAD01             TO        LST-REC.
     WRITE     LST-REC.
     MOVE      SPACE              TO        LST-REC.
     MOVE      HEAD02             TO        LST-REC.
     WRITE     LST-REC.
     MOVE      SPACE              TO        LST-REC.
     MOVE      HEAD03             TO        LST-REC.
     WRITE     LST-REC.
     MOVE      SPACE              TO        LST-REC.
     MOVE      HEAD04             TO        LST-REC.
     WRITE     LST-REC.
*
     MOVE      4                  TO        LINE-CNT.
*
 HEAD-WRT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し
**********************************************************
 MEIWRT-SEC                  SECTION.
*
     MOVE      SPACE              TO        MEISAI01.
*カンマセット
     MOVE      ","                TO        MS01-01K MS01-02K
                                            MS01-03K MS01-04K
                                            MS01-05K MS01-06K
                                            MS01-07K MS01-08K
                                            MS01-09K MS01-10K
                                            MS01-11K MS01-12K
                                            MS01-13K MS01-14K
                                            MS01-15K MS01-16K.
*制御ＣＤセット
     MOVE     X"28"               TO        MS01-03S MS01-08S
                                            MS01-11S MS01-12S
                                            MS01-17S MS01-04S.
     MOVE     X"29"               TO        MS01-03E MS01-08E
                                            MS01-11E MS01-12E
                                            MS01-17E MS01-04E.
*受信日セット
     MOVE      FRA-F01(1:4)       TO        MS01-01(1:4).
     MOVE      FRA-F01(5:2)       TO        MS01-01(6:2).
     MOVE      FRA-F01(7:2)       TO        MS01-01(9:2).
     MOVE      "/"                TO        MS01-01(5:1).
     MOVE      "/"                TO        MS01-01(8:1).
*倉庫情報セット
     MOVE      FRA-F03            TO        MS01-02.
     MOVE      FRA-F03            TO        SOK-F01.
     PERFORM ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG  =  SPACE
         MOVE  SOK-F02            TO        MS01-03
     ELSE
         MOVE  ALL NC"＊"         TO        MS01-03
     END-IF.
*伝票区分
     MOVE      FRA-F04            TO        MS01-04.
     MOVE      ":"                TO        MS01-041.
     MOVE      1                  TO        JYO-F01.
     MOVE      FRA-F04            TO        JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF   HJYOKEN-INV-FLG  =  SPACE
          MOVE JYO-F03            TO        MS01-04N
     ELSE
          MOVE ALL NC"＊"         TO        MS01-04N
     END-IF.
*出荷日
     MOVE FRA-F08(1:4)            TO        MS01-05(1:4).
     MOVE FRA-F08(5:2)            TO        MS01-05(6:2).
     MOVE FRA-F08(7:2)            TO        MS01-05(9:2).
     MOVE "/"                     TO        MS01-05(5:1).
     MOVE "/"                     TO        MS01-05(8:1).
*部門
     MOVE FRA-F101                TO        MS01-06.
*振替情報
     MOVE FRA-F106                TO        MS01-07.
     EVALUATE FRA-F04
        WHEN  70  WHEN  71
         MOVE  FRA-F106           TO        BUM-F01
         PERFORM  BUMONF-READ-SEC
         IF   BUMONF-INV-FLG  =  SPACE
              MOVE  BUM-F02       TO        MS01-08
         ELSE
              MOVE  ALL NC"＊"    TO        MS01-08
         END-IF
*        DISPLAY "AAA" UPON CONS
        WHEN  50  WHEN  51
         MOVE  FRA-F106           TO        SHI-F01
         PERFORM  ZSHIMS-READ-SEC
         IF   ZSHIMS-INV-FLG  =  SPACE
              MOVE  SHI-F02       TO        MS01-08
         ELSE
              MOVE  ALL NC"＊"    TO        MS01-08
         END-IF
*        DISPLAY "BBB" UPON CONS
        WHEN  40  WHEN  41
         MOVE  FRA-F106           TO        TOK-F01
         PERFORM  HTOKMS-READ-SEC
         IF   HTOKMS-INV-FLG  =  SPACE
              MOVE  TOK-F02       TO        MS01-08
         ELSE
              MOVE  ALL NC"＊"    TO        MS01-08
         END-IF
*        DISPLAY "CCC" UPON CONS
        WHEN  30  WHEN  31  WHEN  32  WHEN  35  WHEN  36
         MOVE       SPACE         TO        MS01-08
*        DISPLAY "DDD" UPON CONS
        WHEN  OTHER
         MOVE       SPACE         TO        MS01-08
*        DISPLAY "EEE" UPON CONS
     END-EVALUATE.
*サカタ商品情報
     MOVE FRA-F10A                TO        MS01-09.
     MOVE FRA-F10B                TO        MS01-10.
*商品名検索
     MOVE FRA-F10A                TO        MEI-F011.
     MOVE FRA-F10B(1:5)           TO        MEI-F0121.
     MOVE FRA-F10B(6:2)           TO        MEI-F0122.
     MOVE FRA-F10B(8:1)           TO        MEI-F0123.
     PERFORM HMEIMS-READ-SEC.
     IF  HMEIMS-INV-FLG = "INV"
          MOVE   ALL NC"＊"       TO        MS01-11
     ELSE
          MOVE   MEI-F021         TO        MS01-11(1:15)
          MOVE   MEI-F022         TO        MS01-11(16:15)
     END-IF.
*入出庫
     EVALUATE  FRA-F10G
         WHEN  "1" MOVE NC"入庫"  TO        MS01-12
         WHEN  "2" MOVE NC"出庫"  TO        MS01-12
         WHEN  OTHER MOVE ALL NC"＊" TO     MS01-12
     END-EVALUATE.
*数量
     IF  FRA-F10D  <  ZERO
         MOVE  "-"                TO        MS01-131
     ELSE
         MOVE  "0"                TO        MS01-131
     END-IF.
     MOVE FRA-F10D                TO        WK-SURYO.
     MOVE WK-SURYO1               TO        MS01-13.
     MOVE "."                     TO        MS01-132.
     MOVE WK-SURYO2               TO        MS01-133.
*#2017/03/30 NAV ST
*ストックＮＯ
     MOVE FRA-F10O                TO        MS01-14.
*#2017/03/30 NAV ED
*単価
     MOVE FRA-F10E                TO        WK-SURYO.
     MOVE WK-SURYO1               TO        MS01-15.
     MOVE "."                     TO        MS01-152.
     MOVE WK-SURYO2               TO        MS01-153.
*伝票番号＋行_
     MOVE FRA-F103                TO        MS01-16(1:9)
     MOVE FRA-F104                TO        MS01-16(11:2).
     MOVE "-"                     TO        MS01-16(10:1).
*更新区分
*****IF   FRA-F10T  =  "1"
*         MOVE    NC"○"          TO        MS01-17
*    ELSE
*         MOVE    SPACE           TO        MS01-17
*****END-IF.
     EVALUATE  FRA-F10T
         WHEN  "2"
         MOVE     NC"振替消込分"  TO        MS01-17
         WHEN  "1"
         MOVE     NC"商品エラー"  TO        MS01-17
    ENE-EVALUATE.
*
     MOVE SPACE                   TO        LST-REC.
     MOVE MEISAI01                TO        LST-REC.
     WRITE  LST-REC.
*
     ADD        1                 TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
****************************************************************
*             倉庫マスタ読込
****************************************************************
 ZSOKMS-READ-SEC         SECTION.
*
     READ   ZSOKMS
            INVALID      MOVE   "INV"    TO   ZSOKMS-INV-FLG
            NOT  INVALID MOVE   SPACE    TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             部門マスタ読込
****************************************************************
 BUMONF-READ-SEC         SECTION.
*
     READ   BUMONF
            INVALID      MOVE   "INV"    TO   BUMONF-INV-FLG
            NOT  INVALID MOVE   SPACE    TO   BUMONF-INV-FLG
     END-READ.
*
 BUMONF-READ-EXIT.
     EXIT.
****************************************************************
*             条件ファイル読込
****************************************************************
 HJYOKEN-READ-SEC        SECTION.
*
     READ   HJYOKEN
            INVALID      MOVE   "INV"    TO   HJYOKEN-INV-FLG
            NOT  INVALID MOVE   SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*             仕入先マスタ読込
****************************************************************
 ZSHIMS-READ-SEC         SECTION.
*
     READ   ZSHIMS
            INVALID      MOVE   "INV"    TO   ZSHIMS-INV-FLG
            NOT  INVALID MOVE   SPACE    TO   ZSHIMS-INV-FLG
     END-READ.
*
 ZSHIMS-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込
****************************************************************
 HTOKMS-READ-SEC         SECTION.
*
     READ   HTOKMS
            INVALID      MOVE   "INV"    TO   HTOKMS-INV-FLG
            NOT  INVALID MOVE   SPACE    TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC         SECTION.
*
     READ   HMEIMS
            INVALID      MOVE   "INV"    TO   HMEIMS-INV-FLG
            NOT  INVALID MOVE   SPACE    TO   HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.

```
