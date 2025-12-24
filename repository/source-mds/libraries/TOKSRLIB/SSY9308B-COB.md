# SSY9308B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9308B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　出荷　　　　　　　        *
*    サブシステム　　　　：　ＡＭＡＺＯＮ　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　出荷実績更新ＳＵＢ　　　　　　　　*
*    作成日／作成者　　　：　2020/11/16 NAV INOUE              *
*    処理概要　　　　　　：　パラメタを受け取り、基本情報Ｆ、　*
*                          　売上伝票Ｆの更新を行なう。　　　　*
*                          　また、在庫Ｍの更新も同時に行なう。*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2020/11/16 INOUE 　　新規作成
* 2020/12/23 TAKAHASHI 付番ＦＬＧ更新変更
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY9308B.
*                  流用:SSY3883B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/11/16.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 伝票データ >>--*
     SELECT   SHTDENLA  ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F02   DEN-F04
                                       DEN-F051  DEN-F07
                                       DEN-F112  DEN-F03
                        WITH  DUPLICATES
                        FILE STATUS    DEN-ST.
*---<<  ＡＭＡＺＯＮ基本情報ファイル  >>---*
     SELECT   AMZJOHL3  ASSIGN    TO   DA-01-VI-AMZJOHL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-FE01 JOH-FE02 JOH-FE03
                                       JOH-FB09 JOH-FB11 JOH-FA04
                                       JOH-FB01 JOH-FB02
                        WITH  DUPLICATES
                        FILE STATUS    JOH-ST.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO   DA-01-VI-ZAMZAIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  ZAI-F01
                                       ZAI-F02
                                       ZAI-F03
                        FILE STATUS    ZAI-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   MEIMS1    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F011
                                       MEI-F012
                        FILE STATUS    MEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  SHTDENLA           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< ＡＭＡＺＯＮ基本情報Ｆ >>--*
 FD  AMZJOHL3           LABEL RECORD   IS   STANDARD.
     COPY     AMZJOHL3  OF        XFDLIB
              JOINING   JOH       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF            LABEL RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  MEIMS1-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  AMZJOHL3-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)  VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST            PIC  X(02).
 01  JOH-ST            PIC  X(02).
 01  ZAI-ST            PIC  X(02).
 01  MEI-ST            PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
 01  WRK-NAME.
     03  WRK-KANA            PIC  X(15)        VALUE SPACE.
*
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  LINK-SUB-IN.
     03  LINK-SUB-BUMON    PIC X(04).
     03  LINK-SUB-TANCD    PIC X(02).
     03  LINK-SUB-BTDATE   PIC 9(08).
     03  LINK-SUB-BTTIME   PIC 9(04).
     03  LINK-SUB-BTTOKC   PIC 9(08).
     03  LINK-SUB-SOKOCD   PIC X(02).
     03  LINK-SUB-DENNO    PIC 9(09).
     03  LINK-SUB-GYO      PIC 9(02).
     03  LINK-SUB-TENCD    PIC 9(05).
     03  LINK-SUB-HACDT    PIC 9(08).
     03  LINK-SUB-HACNO    PIC X(10).
     03  LINK-SUB-HACGYO   PIC 9(03).
     03  LINK-SUB-IDCD     PIC X(10).
     03  LINK-SUB-HINCD    PIC X(20).
     03  LINK-SUB-SYKDT    PIC 9(08).
     03  LINK-SUB-NOUDT    PIC 9(08).
     03  LINK-SUB-KONPO    PIC 9(07).
     03  LINK-SUB-JUURYO   PIC 9(10).
     03  LINK-SUB-SSCCCD   PIC X(48).
     03  LINK-SUB-SURKBN   PIC X(01).
     03  LINK-SUB-KENSURY  PIC 9(10).
 01  LINK-SUB-OUT.
     03  LINK-SUB-PGDATE   PIC 9(08).
     03  LINK-SUB-PGTIME   PIC 9(06).
     03  LINK-SUB-KOUSIN1  PIC X(01).
     03  LINK-SUB-KOUSIN2  PIC X(01).
****************************************************************
 PROCEDURE              DIVISION  USING
                        LINK-SUB-IN  LINK-SUB-OUT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 SHTDENLA-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9308B SHTDENLA ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< ＡＭＡＺＯＮ基本情報ファイル >>--*
 AMZJOHL3-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      AMZJOHL3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9308B AMZJOHL3 ERROR " JOH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 在庫マスタ >>--*
 ZAMZAIF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAMZAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9308B ZAMZAIL1 ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 MEIMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MEIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9308B MEIMS1 ERROR " MEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
     ACCEPT   SYS-TIME2      FROM TIME.
*
*****DISPLAY "LINK-SUB-IN   = " LINK-SUB-IN  UPON CONS.
*****DISPLAY "LINK-SUB-OUT  = " LINK-SUB-OUT UPON CONS.
*
     OPEN     I-O       SHTDENLA  AMZJOHL3  ZAMZAIF.
     OPEN     INPUT     MEIMS1.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
     MOVE     SPACE               TO   LINK-SUB-KOUSIN1.
     MOVE     SPACE               TO   LINK-SUB-KOUSIN2.
*ＡＭＡＺＯＮ基本情報Ｆ更新
     MOVE    LINK-SUB-BTDATE      TO   JOH-FE01.
     MOVE    LINK-SUB-BTTIME      TO   JOH-FE02.
     MOVE    LINK-SUB-BTTOKC      TO   JOH-FE03.
     MOVE    LINK-SUB-SOKOCD      TO   JOH-FB09.
     MOVE    LINK-SUB-TENCD       TO   JOH-FB11.
     MOVE    LINK-SUB-HACDT       TO   JOH-FA04.
     MOVE    LINK-SUB-DENNO       TO   JOH-FB01.
     MOVE    LINK-SUB-GYO         TO   JOH-FB02.
*T
*##  DISPLAY "JOH-FE01= " JOH-FE01 UPON CONS.
*##  DISPLAY "JOH-FE02= " JOH-FE02 UPON CONS.
*##  DISPLAY "JOH-FE03= " JOH-FE03 UPON CONS.
*##  DISPLAY "JOH-FB09= " JOH-FB09 UPON CONS.
*##  DISPLAY "JOH-FB11= " JOH-FB11 UPON CONS.
*##  DISPLAY "JOH-FA04= " JOH-FA04 UPON CONS.
*##  DISPLAY "JOH-FB01= " JOH-FB01 UPON CONS.
*##  DISPLAY "JOH-FB02= " JOH-FB02 UPON CONS.
*T
     PERFORM AMZJOHL3-READ-SEC.
     IF      AMZJOHL3-INV-FLG  =  "INV"
             MOVE    SPACE        TO   LINK-SUB-KOUSIN1
             DISPLAY NC"基本情報Ｆなし！？" UPON CONS
             GO                   TO   200-MAIN-RTN-EXIT
     ELSE
*            確定済みの場合は、更新しない。
             IF  JOH-FE14     NOT =    0
                 GO               TO   200-MAIN-RTN-EXIT
             END-IF
*
*            データ区分
             MOVE    "3"               TO   JOH-F001
*            梱包数
             MOVE    LINK-SUB-KONPO    TO   JOH-FC01
*            重量
             MOVE    LINK-SUB-JUURYO   TO   JOH-FC02
*            出荷日
             MOVE    LINK-SUB-SYKDT    TO   JOH-FC03
*            SSCCコード番号
             MOVE    LINK-SUB-SSCCCD   TO   JOH-FC04
*            出荷商品数量
             MOVE    LINK-SUB-KENSURY  TO   JOH-FC05
*            確定（日付）
             MOVE    SYS-DATEW         TO   JOH-FE14
*            確定（時刻）
             MOVE    SYS-TIMEW         TO   JOH-FE15
*            確定（部Ｃ）
             MOVE    LINK-SUB-BUMON    TO   JOH-FE16
*            確定（担Ｃ）
             MOVE    LINK-SUB-TANCD    TO   JOH-FE17
*            納品日
             MOVE    LINK-SUB-NOUDT    TO   JOH-FE18
*
             REWRITE  JOH-REC
             MOVE    "1"          TO   LINK-SUB-KOUSIN1
*****        DISPLAY "DDDDD" UPON CONS
     END-IF.
*
*売上伝票Ｆ更新
     MOVE    LINK-SUB-BTDATE      TO   DEN-F46.
     MOVE    LINK-SUB-BTTIME      TO   DEN-F47.
     MOVE    LINK-SUB-BTTOKC      TO   DEN-F01.
     MOVE    LINK-SUB-SOKOCD      TO   DEN-F48.
     MOVE    LINK-SUB-DENNO       TO   DEN-F02.
     MOVE    0                    TO   DEN-F04.
     MOVE    40                   TO   DEN-F051.
     MOVE    LINK-SUB-TENCD       TO   DEN-F07.
*    MOVE    LINK-SUB-NOUDT       TO   DEN-F112.
     MOVE    LINK-SUB-HACDT       TO   DEN-F112.
     MOVE    LINK-SUB-GYO         TO   DEN-F03.
     PERFORM  SHTDENLA-READ-SEC.
     IF      SHTDENLA-INV-FLG  =  "INV"
             MOVE    SPACE        TO   LINK-SUB-KOUSIN2
             DISPLAY NC"売上伝票Ｆなし！？" UPON CONS
     ELSE
             IF  LINK-SUB-SURKBN  =  "1"
                 IF  LINK-SUB-KENSURY  <=   DEN-F50
                      PERFORM ZAIKO-SEC
                      MOVE   LINK-SUB-KENSURY    TO  DEN-F15
                      COMPUTE DEN-F181 = DEN-F15  *  DEN-F172
                      COMPUTE DEN-F182 = DEN-F15  *  DEN-F173
                 END-IF
             END-IF
             MOVE     LINK-SUB-NOUDT      TO   DEN-F112
*#2020/12/23 NAV ST
*************MOVE    "1"                  TO   DEN-F276
             MOVE     9                   TO   DEN-F276
*#2020/12/23 NAV ED
             MOVE    "1"                  TO   LINK-SUB-KOUSIN2
             REWRITE  DEN-REC
**********   DISPLAY "HHHHH" UPON CONS
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    SHTDENLA  AMZJOHL3  ZAMZAIF  MEIMS1.
*
     MOVE     SYS-DATEW           TO       LINK-SUB-PGDATE.
     MOVE     SYS-TIMEW           TO       LINK-SUB-PGTIME.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ＡＭＡＺＯＮ基本情報Ｆ　読込
*--------------------------------------------------------------*
 AMZJOHL3-READ-SEC       SECTION.
     MOVE    "AMZJOHL3-READ-SEC"   TO   S-NAME.
*
     READ  AMZJOHL3      INVALID
                        MOVE "INV" TO  AMZJOHL3-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  AMZJOHL3-INV-FLG
     END-READ.
*
 AMZJOHL3-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票Ｆ　　　　読込
*--------------------------------------------------------------*
 SHTDENLA-READ-SEC      SECTION.
     MOVE    "SHTDENLA-READ-SEC"  TO   S-NAME.
*
     READ  SHTDENLA     INVALID
                        MOVE "INV" TO  SHTDENLA-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  SHTDENLA-INV-FLG
     END-READ.
*
 SHTDENL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品名称Ｍ　　　　読込
*--------------------------------------------------------------*
 MEIMS1-READ-SEC        SECTION.
     MOVE    "MEIMS1-READ-SEC"    TO   S-NAME.
*
     READ  MEIMS1       INVALID
                        MOVE "INV" TO  MEIMS1-INV-FLG
                        MOVE SPACE TO  WRK-KANA
                   NOT  INVALID
                        MOVE SPACE TO  MEIMS1-INV-FLG
                        MOVE MEI-F031  TO  WRK-KANA
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE   "ZAIKO-SEC"      TO   S-NAME.
     MOVE    SPACE           TO   KEP-FLG.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021  MEI-F011.
     MOVE    DEN-F1412       TO   ZAI-F022  MEI-F012.
     MOVE    DEN-F49         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   MEIMS1-READ-SEC
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
     MOVE      "1"           TO   KEP-FLG.
     IF        MEIMS1-INV-FLG      =   "INV"
               GO  TO   ZAIKO-UPDATE1-EXIT
     END-IF.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      DEN-F08       TO   ZAI-F01.
      MOVE      DEN-F1411     TO   ZAI-F021.
      MOVE      DEN-F1412     TO   ZAI-F022.
      MOVE      DEN-F49       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F27       =    ZAI-F27  +  LINK-SUB-KENSURY
*取引先
      MOVE      DEN-F01       TO   ZAI-F29.
*カナ名称
      MOVE      WRK-KANA      TO   ZAI-F30.
      MOVE      SYS-DATEW     TO   ZAI-F98.
      MOVE      SYS-DATEW     TO   ZAI-F99.
      WRITE     ZAI-REC.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
     INITIALIZE    WRK-AREA2.
*
     IF  DEN-F27D      =     1
*        引当済数に数量減算
         COMPUTE  ZAI-F28   =   ZAI-F28  -  DEN-F15
*        未出庫数に数量減算
         COMPUTE  ZAI-F27   =   ZAI-F27  -  DEN-F15
     ELSE
*        未出庫数に数量減算
         COMPUTE  ZAI-F27   =   ZAI-F27  -  DEN-F15
     END-IF.
*
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  LINK-SUB-KENSURY.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  LINK-SUB-KENSURY
         MOVE     SYS-DATEW       TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =   ZAI-F28  +  LINK-SUB-KENSURY
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  LINK-SUB-KENSURY
         MOVE     SYS-DATEW       TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
