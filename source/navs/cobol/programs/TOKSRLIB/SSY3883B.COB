****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　作場変更、出荷数変更　　　        *
*    モジュール名　　　　：　ナフコ作場数量一括変更更新ＳＵＢ　*
*    作成日／作成者　　　：　2018/12/03 NAV TAKAHASHI          *
*    処理概要　　　　　　：　パラメタを受け取り、ナフコ基本情報*
*                          　Ｆ、売上伝票Ｆの更新を行なう。　　*
*                          　また、在庫Ｍの更新も同時に行なう。*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/12/03 高橋_　　新規作成
* 2019/02/07 高橋_　　売上金額計算追加　　　　　　　　　　
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3883B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/12/03.
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
*---<<  ナフコ基本情報ファイル  >>---*
     SELECT   NFJOHOF   ASSIGN    TO   DA-01-VI-NFJOHOL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-F01  JOH-F05
                                       JOH-F06  JOH-F07
                                       JOH-F08  JOH-F09
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
*---<<  商品変換テーブル  >>---*
     SELECT   SHOTBL1   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TBL-F01   TBL-F02
                        FILE STATUS    TBL-ST.
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
*----<< ナフコ基本情報Ｆ >>--*
 FD  NFJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF            LABEL RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*----<< 商品変換テーブル >>--*
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
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
     03  SHOTBL1-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  NFJOHOF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)  VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST            PIC  X(02).
 01  JOH-ST            PIC  X(02).
 01  ZAI-ST            PIC  X(02).
 01  TBL-ST            PIC  X(02).
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
     03  LINK-SUB-KANRI    PIC 9(08).
     03  LINK-SUB-BTDATE   PIC 9(08).
     03  LINK-SUB-BTTIME   PIC 9(04).
     03  LINK-SUB-BTTOKC   PIC 9(08).
     03  LINK-SUB-SAKUBA   PIC X(02).
     03  LINK-SUB-NOUDT    PIC 9(08).
     03  LINK-SUB-TENCD    PIC 9(05).
     03  LINK-SUB-NFSHOCD  PIC X(08).
     03  LINK-SUB-DENNO    PIC 9(09).
     03  LINK-SUB-GYO      PIC 9(02).
     03  LINK-SUB-SAKKBN   PIC X(01).
     03  LINK-SUB-SURKBN   PIC X(01).
     03  LINK-SUB-HENSAKB  PIC X(02).
     03  LINK-SUB-HENSURY  PIC 9(05).
     03  LINK-SUB-KIHDATE  PIC 9(08).
     03  LINK-SUB-KIHTIME  PIC 9(06).
 01  LINK-SUB-OUT.
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
     DISPLAY  "### SSY3883B SHTDENLA ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< ナフコ基本情報ファイル >>--*
 NFJOHOF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFJOHOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3883B NFJOHOL2 ERROR " JOH-ST " "
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
     DISPLAY  "### SSY3883B ZAMZAIL1 ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 SHOTBL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY3883B SHOTBL1 ERROR " TBL-ST " "
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
     DISPLAY  "### SSY3883B MEIMS1 ERROR " MEI-ST " "
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
*
*****DISPLAY "LINK-SUB-IN   = " LINK-SUB-IN  UPON CONS.
*****DISPLAY "LINK-SUB-OUT  = " LINK-SUB-OUT UPON CONS.
*
     OPEN     I-O       SHTDENLA  NFJOHOF  ZAMZAIF.
     OPEN     INPUT     SHOTBL1   MEIMS1.
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
*ナフコ基本情報Ｆ更新
     MOVE    LINK-SUB-KANRI       TO   JOH-F01.
     MOVE    LINK-SUB-SAKUBA      TO   JOH-F05.
     MOVE    LINK-SUB-TENCD       TO   JOH-F06.
     MOVE    LINK-SUB-DENNO       TO   JOH-F07.
     MOVE    LINK-SUB-GYO         TO   JOH-F08.
     MOVE    LINK-SUB-NOUDT       TO   JOH-F09.
*****DISPLAY "JOH-F01 = " JOH-F01  UPON CONS.
*    DISPLAY "JOH-F05 = " JOH-F05  UPON CONS.
*    DISPLAY "JOH-F06 = " JOH-F06  UPON CONS.
*    DISPLAY "JOH-F07 = " JOH-F07  UPON CONS.
*    DISPLAY "JOH-F08 = " JOH-F08  UPON CONS.
*****DISPLAY "JOH-F09 = " JOH-F09  UPON CONS.
     PERFORM NFJOHOF-READ-SEC.
     IF      NFJOHOF-INV-FLG  =  "INV"
             MOVE    SPACE        TO   LINK-SUB-KOUSIN1
*****        DISPLAY "AAAAA" UPON CONS
             GO                   TO   200-MAIN-RTN-EXIT
     ELSE
*************発注確定済みの場合は、更新しない。
             IF  JOH-F34          =  "1"
                 GO               TO   200-MAIN-RTN-EXIT
             END-IF
             IF  LINK-SUB-SAKKBN  =  "1"
                 MOVE   LINK-SUB-HENSAKB    TO  JOH-F05
*****        DISPLAY "BBBBB" UPON CONS
             END-IF
             IF  LINK-SUB-SURKBN  =  "1"
                 MOVE   LINK-SUB-HENSURY    TO  JOH-F20
*****        DISPLAY "CCCCC" UPON CONS
             END-IF
             MOVE    "1"          TO   LINK-SUB-KOUSIN1
             REWRITE  JOH-REC
*****        DISPLAY "DDDDD" UPON CONS
     END-IF.
*
*売上伝票Ｆ更新
     MOVE    LINK-SUB-BTDATE      TO   DEN-F46.

     MOVE    LINK-SUB-BTTIME      TO   DEN-F47.
     MOVE    LINK-SUB-BTTOKC      TO   DEN-F01.
     MOVE    LINK-SUB-SAKUBA      TO   DEN-F48.
     MOVE    LINK-SUB-DENNO       TO   DEN-F02.
     MOVE    0                    TO   DEN-F04.
     MOVE    40                   TO   DEN-F051.
     MOVE    LINK-SUB-TENCD       TO   DEN-F07.
     MOVE    LINK-SUB-NOUDT       TO   DEN-F112.
     MOVE    LINK-SUB-GYO         TO   DEN-F03.
     PERFORM  SHTDENLA-READ-SEC.
     IF      SHTDENLA-INV-FLG  =  "INV"
             MOVE    SPACE        TO   LINK-SUB-KOUSIN2
***********  DISPLAY "EEEEE" UPON CONS
     ELSE
             IF  LINK-SUB-SAKKBN  =  "1"
                 MOVE   LINK-SUB-HENSAKB    TO  DEN-F48
                 MOVE   SPACE               TO  DEN-F68  DEN-F69
*********    DISPLAY "FFFFF" UPON CONS
             END-IF
             IF  LINK-SUB-SURKBN  =  "1"
                 IF  LINK-SUB-HENSURY  <=   DEN-F50
                      PERFORM ZAIKO-SEC
                      MOVE   LINK-SUB-HENSURY    TO  DEN-F15
                      MOVE   SPACE         TO  DEN-F68  DEN-F69
**********************2019/02/07 NAV ST
                      COMPUTE DEN-F181 = DEN-F15  *  DEN-F172
                      COMPUTE DEN-F182 = DEN-F15  *  DEN-F173
**********************2019/02/07 NAV ED
******************    DISPLAY "GGGGG" UPON CONS
                 END-IF
             END-IF
             MOVE    "1"          TO   LINK-SUB-KOUSIN2
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
     CLOSE    SHTDENLA  NFJOHOF  ZAMZAIF  SHOTBL1  MEIMS1.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ナフコ基本情報Ｆ　読込
*--------------------------------------------------------------*
 NFJOHOF-READ-SEC       SECTION.
     MOVE    "NFJOHOF-READ-SEC"   TO   S-NAME.
*
     READ  NFJOHOF      INVALID
                        MOVE "INV" TO  NFJOHOF-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  NFJOHOF-INV-FLG
     END-READ.
*
 NFJOHOF-READ-EXIT.
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
*    商品変換ＴＢＬ　　読込
*--------------------------------------------------------------*
 SHOTBL1-READ-SEC       SECTION.
     MOVE    "SHOTBL1-READ-SEC"   TO   S-NAME.
*
     READ  SHOTBL1      INVALID
                        MOVE "INV" TO  SHOTBL1-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  SHOTBL1-INV-FLG
     END-READ.
*
 SHOTBL1-READ-EXIT.
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
      COMPUTE   ZAI-F27       =    ZAI-F27  +  LINK-SUB-HENSURY
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
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  LINK-SUB-HENSURY.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  LINK-SUB-HENSURY
         MOVE     SYS-DATEW       TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =   ZAI-F28  +  LINK-SUB-HENSURY
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  LINK-SUB-HENSURY
         MOVE     SYS-DATEW       TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
