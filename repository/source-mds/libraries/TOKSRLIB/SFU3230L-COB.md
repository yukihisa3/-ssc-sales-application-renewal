# SFU3230L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3230L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＡＣＯＳ振替改善　　　　　　　　　*
*    モジュール名　　　　：　振替更新データリスト発行　　　　　*
*    作成日／更新日　　　：　2016/01/06                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタを受取り、振替データ累積  *
*                        ：　ファイルよりリスト出力を行なう。  *
*    更新日／更新者　　　：　2017/03/30 NAV TAKAHASHI          *
*    更新概要　　　　　　：　ストック_項目を追加　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SFU3230L.
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
     SELECT      ZSOKMS      ASSIGN    TO       DA-01-VI-ZSOKMS1          
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      SOK-F01                   
                             FILE      STATUS   SOK-ST.                   
*----<< 部門マスタ >>--*                                                
     SELECT      BUMONF      ASSIGN    TO       DA-01-VI-BUMONL1          
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      BUM-F01                   
                             FILE      STATUS   BUM-ST.                   
*----<< 条件ファイル >>--*                                              
     SELECT      HJYOKEN     ASSIGN    TO       DA-01-VI-JYOKEN1          
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      JYO-F01  JYO-F02          
                             FILE      STATUS   JYO-ST.                   
*----<< 仕入先マスタ >>--*                                              
     SELECT      ZSHIMS      ASSIGN    TO       DA-01-VI-ZSHIMS1          
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      SHI-F01                   
                             FILE      STATUS   SHI-ST.                   
*----<< 取引先マスタ >>--*                                              
     SELECT      HTOKMS      ASSIGN    TO       DA-01-VI-TOKMS2           
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      TOK-F01                   
                             FILE      STATUS   TOK-ST.                   
*----<< 商品名称マスタ >>--*                                            
     SELECT      HMEIMS      ASSIGN    TO       DA-01-VI-MEIMS1           
                             ORGANIZATION       INDEXED                   
                             ACCESS    MODE     RANDOM                    
                             RECORD    KEY      MEI-F011                  
                                                MEI-F0121                 
                                                MEI-F0122                 
                                                MEI-F0123                 
                             FILE      STATUS   MEI-ST.                   
*----<< ﾌﾟﾘﾝﾀｰ >>--*
     SELECT     PRTF         ASSIGN    TO       LP-04-PRTF
                             FILE      STATUS   PRT-ST.                   
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
*----<< ﾌﾟﾘﾝﾀｰ >>--*
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
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
     03  PRT-ST              PIC  X(02).
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
     03  PRT-ERR             PIC  N(15) VALUE
         NC"プリンターエラー".
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
*帳票出力定義エリア
****　見出し行１
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SFU3230L".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  HDKBN               PIC  N(07)
         CHARACTER  TYPE  IS  YA.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(18)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　振替更新データリスト　＊＊＊".
     02  FILLER              PIC  X(22)  VALUE  SPACE.
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"年".
     02  SYSMM               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"月".
     02  SYSDD               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"日".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SYSTM               PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  LPAGE               PIC  ZZ9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"頁".
****  見出し行２
 01  MIDASI-2.
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"受信日：".
     02  JUSHINBI            PIC  X(10).
*
****  見出し行３
 01  MIDASI-3.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(05)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"倉庫情報：".
     02  HDSOKCD             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  HDSOKNM             PIC  N(10)
         CHARACTER  TYPE  IS  YA.
     02  FILLER              PIC  X(02)  VALUE " (".
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE NC"受".
     02  FILLER              PIC  X(01)  VALUE ":".
     02  HDJYUST             PIC  99999999.
     02  FILLER              PIC  X(01)  VALUE "-".
     02  HDJYUED             PIC  99999999.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE NC"倉".
     02  FILLER              PIC  X(01)  VALUE ":".
     02  HDSOKST             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE "-".
     02  HDSOKED             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE NC"出".
     02  FILLER              PIC  X(01)  VALUE ":".
     02  HDSYUST             PIC  99999999.
     02  FILLER              PIC  X(01)  VALUE "-".
     02  HDSYUED             PIC  99999999.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE NC"商".
     02  FILLER              PIC  X(01)  VALUE ":".
     02  HDSYOST             PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE "-".
     02  HDSYOED             PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE NC"伝".
     02  FILLER              PIC  X(01)  VALUE ":".
     02  HDDENK1             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE ",".
     02  HDDENK2             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE ",".
     02  HDDENK3             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE ",".
     02  HDDENK4             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE ",".
     02  HDDENK5             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE ",".
     02  HDDENK6             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE ",".
     02  HDDENK7             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE ",".
     02  HDDENK8             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE ")".
*
****  見出し行４
 01  MIDASI-4.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"伝票区分".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"出荷日".
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  FILLER              PIC  N(02)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"部門".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"振替情報".
     02  FILLER              PIC  X(19)  VALUE  SPACE.
     02  FILLER              PIC  N(07)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"サカタ商品情報".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"商品名".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(02)
         CHARACTER  TYPE  IS  YB         VALUE
         NC"入出".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"数　量".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "(ｽﾄｯｸNO)".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"単　価".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(05)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"伝票_＋行".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"更".
*
****  線１
 01  HASEN-1.
     02  FILLER              PIC  X(136) VALUE  ALL "=".
****  線２
 01  HASEN-2.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
*
****  明細行
 01  MEISAI-1.
     02  MSDENKU             PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSDENKUNM           PIC  N(04)
         CHARACTER  TYPE  IS  YB.
     02  MSSYUKABI           PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSBUMON             PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSFURIKAE1          PIC  X(01).
     02  MSFURIKAECD         PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSFURIKAENM         PIC  N(10)
         CHARACTER  TYPE  IS  YB.
     02  MSFURIKAE2          PIC  X(01)  VALUE  ")".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSSYOHINCD          PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSHINTANCD          PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSSYOHINNM          PIC  N(10)
         CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSNYUSYUKOKBN       PIC  N(02)
         CHARACTER  TYPE  IS  YB.
     02  MSSURYO             PIC  ---,--9.99.
     02  MSSTNO              PIC  X(08)  VALUE  SPACE.
     02  MSTANKA             PIC  ---,--9.99.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSDENPYO            PIC  X(12).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MSKOUSIN            PIC  N(01)
         CHARACTER  TYPE  IS  YA.
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
 01  PARA-OUTKBN             PIC  X(01).
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
                                            PARA-OUTKBN.
****************************************************************
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
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
                 OUTPUT    PRTF.
*----<< ｺﾃｲﾁ ｾｯﾄ >--*
     MOVE      58                 TO   LINE-CNT.
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE      ZERO               TO   PAGE-CNT.
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
     PERFORM  FRACOSF-READ-SEC.
     IF  END-FLG  =  "END"
         DISPLAY NC"＃＃対象データ無（ＲＤ）＃＃" UPON CONS
     END-IF.
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
 READ070.
*伝票区分チェック
 READ071.
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
 READ072.
     IF  PARA-OUTKBN NOT = SPACE
         IF   PARA-OUTKBN = "1"
              IF  FRA-F10T NOT = "2"
                  GO       TO   FRACOSF-READ-SEC
              END-IF
         ELSE
              IF  PARA-OUTKBN = "2"
                  IF  FRA-F10T NOT = "1"
                      GO   TO   FRACOSF-READ-SEC
                  END-IF
              END-IF
         END-IF
     END-IF.
 READ080.
*最新キーセット
     MOVE  FRA-F01         TO   NEW-JYUSIN.
     MOVE  FRA-F03         TO   NEW-SOKCD.
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
*
     IF    NEW-KEY    NOT =  OLD-KEY
           PERFORM  HEAD-WRT-SEC
           MOVE   NEW-KEY    TO      OLD-KEY
     END-IF.
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
                 PRTF.
*
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し
**********************************************************
 HEAD-WRT-SEC                  SECTION.
*頁が０以上の時、改頁実施
     IF  PAGE-CNT  >  ZERO
         MOVE      SPACE          TO        P-REC
         WRITE     P-REC         AFTER      PAGE
     END-IF.
*----<< ﾋﾂﾞｹ ｼﾞｶﾝ >>--*
     MOVE      SYS-DATE(1:4)      TO        SYSYY.
     MOVE      WK-M               TO        SYSMM.
     MOVE      WK-D               TO        SYSDD.
     MOVE      WK-TIME(1:2)       TO        SYSTM(1:2).
     MOVE      ":"                TO        SYSTM(3:1).
     MOVE      WK-TIME(3:2)       TO        SYSTM(4:2).
     MOVE      ":"                TO        SYSTM(6:1).
     MOVE      WK-TIME(5:2)       TO        SYSTM(7:2).
*頁カウント
     ADD       1                  TO        PAGE-CNT.
     MOVE      PAGE-CNT           TO        LPAGE.
*行カウント初期化
     MOVE      ZERO               TO        LINE-CNT.
*見出し：受信日セット
     MOVE      FRA-F01(1:4)       TO        JUSHINBI(1:4).
     MOVE      FRA-F01(5:2)       TO        JUSHINBI(6:2).
     MOVE      FRA-F01(7:2)       TO        JUSHINBI(9:2).
     MOVE      "/"                TO        JUSHINBI(5:1).
     MOVE      "/"                TO        JUSHINBI(8:1).
*見出し：倉庫情報セット
     MOVE      FRA-F03            TO        HDSOKCD
     MOVE      FRA-F03            TO        SOK-F01.
     PERFORM ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG  =  SPACE
         MOVE  SOK-F02            TO        HDSOKNM
     ELSE
         MOVE  ALL NC"＊"         TO        HDSOKNM
     END-IF.
*出力指定条件セット
     MOVE      PARA-JDTST         TO        HDJYUST.
     MOVE      PARA-JDTED         TO        HDJYUED.
     MOVE      PARA-SOKST         TO        HDSOKST.
     MOVE      PARA-SOKED         TO        HDSOKED.
     MOVE      PARA-SKDST         TO        HDSYUST.
     MOVE      PARA-SKDED         TO        HDSYUED.
     MOVE      PARA-SYOST         TO        HDSYOST.
     MOVE      PARA-SYOED         TO        HDSYOED.
     MOVE      PARA-DENK1         TO        HDDENK1.
     MOVE      PARA-DENK2         TO        HDDENK2.
     MOVE      PARA-DENK3         TO        HDDENK3.
     MOVE      PARA-DENK4         TO        HDDENK4.
     MOVE      PARA-DENK5         TO        HDDENK5.
     MOVE      PARA-DENK6         TO        HDDENK6.
     MOVE      PARA-DENK7         TO        HDDENK7.
     MOVE      PARA-DENK8         TO        HDDENK8.
*出力区分セット
     EVALUATE  PARA-OUTKBN
         WHEN  SPACE
         MOVE  NC"（　全　て　）" TO        HDKBN
         WHEN  "1"
         MOVE  NC"（振替消込分）" TO        HDKBN
         WHEN  "2"
         MOVE  NC"（エラー分　）" TO        HDKBN
         WHEN  OTHER
         MOVE  NC"＊＊＊＊＊＊＊" TO        HDKBN
     END-EVALUATE.
**************
*帳票書き出し*
**************
     WRITE     P-REC   FROM    MIDASI-1     AFTER  2.
     WRITE     P-REC   FROM    MIDASI-2     AFTER  2.
     WRITE     P-REC   FROM    MIDASI-3     AFTER  1.
     WRITE     P-REC   FROM    HASEN-1      AFTER  1.
     WRITE     P-REC   FROM    MIDASI-4     AFTER  1.
     WRITE     P-REC   FROM    HASEN-1      AFTER  1.
*****MOVE      SPACE              TO        P-REC.
*****WRITE     P-REC   AFTER      1.
*
     MOVE      9                  TO        LINE-CNT.
*
 HEAD-WRT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し
**********************************************************
 MEIWRT-SEC                  SECTION.
*指定行数以上になった場合、改頁へ
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
     MOVE      SPACE              TO        MEISAI-1.
*伝票区分
     MOVE      FRA-F04            TO        MSDENKU.
     MOVE      1                  TO        JYO-F01.
     MOVE      FRA-F04            TO        JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF   HJYOKEN-INV-FLG  =  SPACE
          MOVE JYO-F03            TO        MSDENKUNM
     ELSE
          MOVE ALL NC"＊"         TO        MSDENKUNM
     END-IF.
*出荷日
     MOVE FRA-F08(1:4)            TO        MSSYUKABI(1:4).
     MOVE FRA-F08(5:2)            TO        MSSYUKABI(6:2).
     MOVE FRA-F08(7:2)            TO        MSSYUKABI(9:2).
     MOVE "/"                     TO        MSSYUKABI(5:1).
     MOVE "/"                     TO        MSSYUKABI(8:1).
*部門
     MOVE FRA-F101                TO        MSBUMON.
*振替情報
     MOVE "("                     TO        MSFURIKAE1.
     MOVE ")"                     TO        MSFURIKAE2.
     MOVE FRA-F106                TO        MSFURIKAECD.
     EVALUATE FRA-F04
        WHEN  70  WHEN  71
         MOVE  FRA-F106           TO        BUM-F01
         PERFORM  BUMONF-READ-SEC
         IF   BUMONF-INV-FLG  =  SPACE
              MOVE  BUM-F02       TO        MSFURIKAENM
         ELSE
              MOVE  ALL NC"＊"    TO        MSFURIKAENM
         END-IF
*        DISPLAY "AAA" UPON CONS
        WHEN  50  WHEN  51
         MOVE  FRA-F106           TO        SHI-F01
         PERFORM  ZSHIMS-READ-SEC
         IF   ZSHIMS-INV-FLG  =  SPACE
              MOVE  SHI-F02       TO        MSFURIKAENM
         ELSE
              MOVE  ALL NC"＊"    TO        MSFURIKAENM
         END-IF
*        DISPLAY "BBB" UPON CONS
        WHEN  40  WHEN  41
         MOVE  FRA-F106           TO        TOK-F01
         PERFORM  HTOKMS-READ-SEC
         IF   HTOKMS-INV-FLG  =  SPACE
              MOVE  TOK-F02       TO        MSFURIKAENM
         ELSE
              MOVE  ALL NC"＊"    TO        MSFURIKAENM
         END-IF
*        DISPLAY "CCC" UPON CONS
        WHEN  30  WHEN  31  WHEN  32  WHEN  35  WHEN  36
         MOVE       SPACE         TO        MSFURIKAENM
*        DISPLAY "DDD" UPON CONS
        WHEN  OTHER
         MOVE       SPACE         TO        MSFURIKAENM
*        DISPLAY "EEE" UPON CONS
     END-EVALUATE.
*サカタ商品情報
     MOVE FRA-F10A                TO        MSSYOHINCD.
     MOVE FRA-F10B(1:5)           TO        MSHINTANCD(1:5).
     MOVE FRA-F10B(6:2)           TO        MSHINTANCD(7:2).
     MOVE FRA-F10B(8:1)           TO        MSHINTANCD(10:1).
     MOVE "-"                     TO        MSHINTANCD(6:1).
     MOVE "-"                     TO        MSHINTANCD(9:1).
*商品名検索
     MOVE FRA-F10A                TO        MEI-F011.
     MOVE FRA-F10B(1:5)           TO        MEI-F0121.
     MOVE FRA-F10B(6:2)           TO        MEI-F0122.
     MOVE FRA-F10B(8:1)           TO        MEI-F0123.
     PERFORM HMEIMS-READ-SEC.
     IF  HMEIMS-INV-FLG = "INV"
          MOVE   ALL NC"＊"       TO        MSSYOHINNM
     ELSE
          MOVE   MEI-F021         TO        MSSYOHINNM
     END-IF.
*入出庫
     EVALUATE  FRA-F10G
         WHEN  "1" MOVE NC"入庫"  TO        MSNYUSYUKOKBN
         WHEN  "2" MOVE NC"出庫"  TO        MSNYUSYUKOKBN
         WHEN  OTHER MOVE ALL NC"＊" TO     MSNYUSYUKOKBN
     END-EVALUATE.
*数量
     MOVE FRA-F10D                TO        MSSURYO.
*##2017/03/30 NAV ST
*ストック_
     MOVE "("                     TO        MSSTNO(1:1).
     MOVE FRA-F10O                TO        MSSTNO(2:6).
     MOVE ")"                     TO        MSSTNO(8:1).
*##2017/03/30 NAV ED
*単価
     MOVE FRA-F10E                TO        MSTANKA.
*伝票番号＋行_
     MOVE FRA-F103                TO        MSDENPYO(1:9).
     MOVE FRA-F104                TO        MSDENPYO(11:2).
     MOVE "-"                     TO        MSDENPYO(10:1).
*更新区分
     IF   FRA-F10T  =  "1"
          MOVE    NC"○"          TO        MSKOUSIN
     ELSE
          MOVE    SPACE           TO        MSKOUSIN
     END-IF.
     EVALUATE   FRA-F10T
         WHEN   SPACE  MOVE SPACE TO        MSKOUSIN
         WHEN   "1"    MOVE NC"Ｅ" TO       MSKOUSIN
         WHEN   "2"    MOVE NC"消" TO       MSKOUSIN
         WHEN   OTHER  MOVE NC"＊" TO       MSKOUSIN
     END-EVALUATE.
*
     WRITE      P-REC   FROM      MEISAI-1  AFTER  1.
     WRITE      P-REC   FROM      HASEN-2   AFTER  1.
     ADD        2                 TO        LINE-CNT.
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
