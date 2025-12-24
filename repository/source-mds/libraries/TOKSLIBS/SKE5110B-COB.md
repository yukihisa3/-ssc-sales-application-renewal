# SKE5110B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKE5110B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品サブシステム              *
*    業務名　　　　　　　：　出荷検品                          *
*    モジュール名　　　　：　ロケーションマスタ送信作成        *
*    作成日／更新日　　　：　2008/02/14                        *
*    作成者／更新者　　　：　ＮＡＶ阿部　　　　　　　　　　　　*
*    処理概要　　　　　　：　倉庫別検品取引先設定Ｍを順読みし、*
*                            倉庫毎に登録されている取引先_番を*
*                            変換マスタより抽出し、条件Ｆの出力*
*                            Ｆ番号に従い、追加出力する。      *
*    2008/08/11 項目に取引先ＣＤを追加                         *
*    2017/07/18 検品Ｇ＝数字以外の場合、０をセットに変更　　　 *
*    2023/10/23 相手商品ＣＤの７桁目が空白の場合、抽出対象×　 *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE5110B.
 AUTHOR.                Y.ABE.
 DATE-WRITTEN.          08/02/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA       IS        YA
     YB-21    IS        YB-21
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*倉庫別検品取引先設定マスタ
     SELECT   SOKKENF   ASSIGN    TO        DA-01-VI-SOKKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SOK-F01   SOK-F02
                        FILE  STATUS   IS   SOK-STATUS.
*商品変換テーブル
     SELECT   HSHOTBL    ASSIGN    TO        DA-01-VI-SHOTBL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SHO-F01
                                            SHO-F04
                                            SHO-F02
                        FILE STATUS    IS   SHO-STATUS.
*商品名称マスタ
     SELECT   HMEIMS     ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE STATUS    IS   MEI-STATUS.
*ＳＵＢ商品名称マスタ
     SELECT   SUBMEIL1 ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F011
                                                 SUB-F0121
                                                 SUB-F0122
                                                 SUB-F0123
                        FILE      STATUS    IS   SUB-STATUS.
*送信用データ１
     SELECT   KNPTAN01  ASSIGN    TO        KNPTAN01
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D01-STATUS.
*件数ファイル０１
     SELECT   KNPTAK01  ASSIGN    TO        KNPTAK01
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K01-STATUS.
*プリント Ｆ
     SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    倉庫別検品取引先設定マスタ
******************************************************************
 FD  SOKKENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SOKKENF   OF        XFDLIB
              JOINING   SOK  AS   PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL             LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL    OF        XFDLIB
              JOINING   SHO       PREFIX.
*
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS              LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS     OF        XFDLIB
              JOINING   MEI       PREFIX.
*
******************************************************************
*    ＳＵＢ商品名称マスタ
******************************************************************
*
 FD  SUBMEIL1.
     COPY     SUBMEIF  OF        XFDLIB
              JOINING   SUB       PREFIX.
******************************************************************
*    送信用店舗データ１
******************************************************************
 FD  KNPTAN01           LABEL RECORD   IS   STANDARD.
     COPY     KNPTANFN  OF        XFDLIB
              JOINING   D01       PREFIX.
******************************************************************
*    件数データ０１
******************************************************************
 FD  KNPTAK01           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K01-REC.
     03  K01-F01             PIC  9(08).
     03  K01-F02             PIC  X(02).
****************************************************************
*    FILE = プリント　ファイル                                 *
****************************************************************
 FD  PRINTF.
 01  PRINT-REC                    PIC       X(200).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  TBL-FLG                 PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  SUBMEIL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  WK-SOKCD                PIC  X(02)     VALUE  SPACE.
 01  NUM-CNT                 PIC  9(08)     VALUE  ZERO.
 01  NUM-CNT1                PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SOK-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  D01-STATUS        PIC  X(02).
     03  K01-STATUS        PIC  X(02).
     03  SUB-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKE5110B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE5110B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE5110B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  WK-FLCD.
         05  WK-FLCDT       PIC  9(05)  VALUE  ZERO.
     03  WK-RTCD.
         05  WK-RTCDT       PIC  X(02)  VALUE  ZERO.
     03  WK-RTNM.
         05  WK-RTNMT       PIC  N(10)  VALUE  SPACE.
     03  WK-RTNM.
         05  WK-RTCNTT      PIC  9(05)  VALUE  ZERO.
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(36)  VALUE SPACE.
     03  FILLER                   PIC       N(18)  VALUE
       NC"【　出荷検品変換Ｔ振分件数リスト　】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(18)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "DATE:".
     03  YY                       PIC       99.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  MM                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  DD                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "PAGE:".
     03  PEIJI                    PIC       ZZZ9.
*    見出し行２
 01  MIDASHI2           CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(40)  VALUE SPACE.
     03  FILLER                   PIC       X(08)  VALUE
         "FILE-NO.".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"場所ＣＤ".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"出荷場所名".
     03  FILLER                   PIC       X(12)  VALUE SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"データ件数".
     03  FILLER                   PIC       X(78)  VALUE SPACE.
*    明細行
 01  MEISAI             CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(41)  VALUE SPACE.
     03  FILECD                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(08)  VALUE SPACE.
     03  ROUTECD                  PIC       X(02).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  ROUTENM                  PIC       N(10).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  DATASU                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(78)  VALUE SPACE.
*    線１
 01  SEN1               CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(18)  VALUE
         NC"──────────────────".
*    線２
 01  SEN2.
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(36)  VALUE
         "------------------------------------".
*
     COPY   KNPTANFN OF XFDLIB  JOINING   WK  AS   PREFIX.
*    名称編集１
 01  WK-HEN1.
     03  WK-HEN1-1          PIC   X(01).
     03  WK-HEN1-2          PIC   N(15).
     03  WK-HEN1-3          PIC   X(01).
*    名称編集２
 01  WK-HEN2.
     03  WK-HEN2-1          PIC   X(01).
     03  WK-HEN2-2          PIC   N(10).
     03  WK-HEN2-3          PIC   X(01).
*    名称編集２
 01  WK-HEN-DATE.
     03  WK-HEN-DATE1       PIC   X(04).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE2       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE3       PIC   X(02).
*    日付変換１
 01  WK-HENKAN              PIC   9(08)  VALUE  ZERO.
 01  WK-TOUROKU.
     03  WK-TOUROKU1        PIC   9(04).
     03  WK-TOUROKU2        PIC   9(02).
     03  WK-TOUROKU3        PIC   9(02).
*    日付変換２
 01  WK-KOUSIN.
     03  WK-KOUSIN1         PIC   9(04).
     03  WK-KOUSIN2         PIC   9(02).
     03  WK-KOUSIN3         PIC   9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-SOKCD             PIC   X(02).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  PARA-SOKCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SOKKENF.
     MOVE      "SOKKENF "   TO   AB-FILE.
     MOVE      SOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPTAN01.
     MOVE      "KNPTAN01"   TO   AB-FILE.
     MOVE      D01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "HSHOTBL  "   TO   AB-FILE.
     MOVE      SHO-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "HMEIMS   "   TO   AB-FILE.
     MOVE      MEI-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPTAK01.
     MOVE      "KNPTAK01"   TO   AB-FILE.
     MOVE      K01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBMEIL1.
     MOVE      "SUBMEIL1"   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SOKKENF HSHOTBL HMEIMS SUBMEIL1.
     OPEN     OUTPUT    KNPTAN01.
     OPEN     OUTPUT    KNPTAK01.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG    RD-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*    倉庫別検品取引先マスタスタート
     MOVE     PARA-SOKCD TO    SOK-F01.
     MOVE     ZERO       TO    SOK-F02.
     START    SOKKENF  KEY  IS  >=  SOK-F01 SOK-F02
              INVALID
              GO         TO    INIT-EXIT
     END-START.
*    倉庫別検品取引先マスタ順読み
     PERFORM     SOKKENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    終了条件チェック
     IF     ( SOK-F01  NOT =  PARA-SOKCD )
*             終了条件セット
              MOVE     "END"    TO      END-FLG
              GO         TO    MAIN-EXIT
     END-IF.
*    商品変換テーブルスタート
     MOVE     SPACE      TO    TBL-FLG.
     MOVE     SOK-F02    TO    SHO-F01.
     MOVE     SOK-F01    TO    SHO-F04.
***  DISPLAY "AAA SOK-F01 = " SOK-F01  UPON CONS.
***  DISPLAY "AAA SOK-F02 = " SOK-F02  UPON CONS.
*20230731 NAV ST　さくらでコーナンの時はすべて転送する。
     IF       SOK-F01  =  "TK"
     AND      SOK-F02  =  2363
              MOVE SPACE TO    SHO-F04
***   DISPLAY "AAA" UPON CONS
     END-IF.
*20230731 NAV ED
     MOVE     SPACE      TO    SHO-F02.
     START    HSHOTBL  KEY  IS  >=  SHO-F01 SHO-F04 SHO-F02
              INVALID
              GO         TO    MAIN-010
     END-START.
     DISPLAY "BBB" UPON CONS.
*    商品変換テーブル出力（１件目）
     PERFORM  HSHOTBL-READ-SEC.
     DISPLAY "CCC" UPON CONS.
*    店舗情報出力
     PERFORM  UNTIL  TBL-FLG = "END"
*             商品変換情報セット
              PERFORM TBL-SET-SEC
*             ファイル出力テーブル判定
              PERFORM TBLSET-SEC
*             商品変換テーブル読込み
              PERFORM HSHOTBL-READ-SEC
     END-PERFORM.
     DISPLAY "DDD" UPON CONS.
*
 MAIN-010.
*    倉庫別検品取引先設定マスタ読込み
     PERFORM  SOKKENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*出力件数リスト出力
     PERFORM   LISTWT-SEC.
*検品Ｇ数字以外件数
     DISPLAY "NOT NUMERIC-CNT  KEIYO = " NUM-CNT  UPON CONS.
     DISPLAY "NOT NUMERIC-CNT        = " NUM-CNT1 UPON CONS.
*
     CLOSE     SOKKENF  HSHOTBL  HMEIMS SUBMEIL1.
     CLOSE     KNPTAN01.
     CLOSE     KNPTAK01.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫出力先ファイル判定→ファイル出力            *
****************************************************************
 TBL-SET-SEC         SECTION.
*
     MOVE     "TBL-SET-SEC"   TO      S-NAME.
*    ワークエリア初期化
     MOVE      SPACE            TO      WK-REC.
     INITIALIZE                         WK-REC.
*    項目セット
*    倉庫ＣＤ
     MOVE      SHO-F04          TO      WK-F01.
*20230731 NAV ST　さくらでコーナンの時はすべて転送する。
     IF       SOK-F01  =  "TK"
     AND      SOK-F02  =  2363
              MOVE "TK"         TO      WK-F01
     END-IF.
*20230731 NAV ED
*    _番
     MOVE      SHO-F08          TO      WK-F02.
*    検品ＧＣＤ
     MOVE      SHO-F11          TO      WK-F03.
     IF   SHO-F11 NOT NUMERIC
               MOVE  ZERO       TO      WK-F03
               IF  SHO-F01 = 173
*******************DISPLAY "SHO-F02 = " SHO-F02 UPON CONS
                   ADD   1          TO      NUM-CNT
               ELSE
                   ADD   1          TO      NUM-CNT1
               END-IF
     END-IF.
*    自社商品ＣＤ
     MOVE      SHO-F03          TO      WK-F04.
*    ＪＡＮＣＤ
     MOVE      SHO-F02          TO      WK-F05.
     IF        SHO-F01  =  38709  OR  116501
               MOVE  SHO-F03    TO      MEI-F01
               PERFORM HMEIMS-READ-SEC
               IF    HMEIMS-INV-FLG = SPACE
                     MOVE MEI-F06   TO  WK-F05
               ELSE
                     DISPLAY NC"ＪＡＮＣＤなし" " = " SHO-F03
                             UPON CONS
               END-IF
     END-IF.
**   IF        SHO-F01  =  116501
*              MOVE  SHO-F031   TO      SUB-F011
*              MOVE  SHO-F0321  TO      SUB-F0121
*              MOVE  SHO-F0322  TO      SUB-F0122
*              MOVE  SHO-F0323  TO      SUB-F0123
*              PERFORM SUBMEIL1-READ-SEC
*              IF    SUBMEIL1-INV-FLG = SPACE
*                    MOVE SUB-D01   TO  WK-F05
*              ELSE
*                    DISPLAY NC"ＪＡＮＣＤなし" " = " SHO-F03
*                            UPON CONS
*              END-IF
**   END-IF.
*    登録日付
     MOVE      SHO-F98          TO      WK-HENKAN.
     MOVE      WK-HENKAN        TO      WK-TOUROKU.
     MOVE      WK-TOUROKU1      TO      WK-HEN-DATE1.
     MOVE      WK-TOUROKU2      TO      WK-HEN-DATE2.
     MOVE      WK-TOUROKU3      TO      WK-HEN-DATE3.
     MOVE      WK-HEN-DATE      TO      WK-F06.
*    更新日付
     MOVE      SHO-F99          TO      WK-HENKAN.
     MOVE      WK-HENKAN        TO      WK-KOUSIN.
     MOVE      WK-KOUSIN1       TO      WK-HEN-DATE1.
     MOVE      WK-KOUSIN2       TO      WK-HEN-DATE2.
     MOVE      WK-KOUSIN3       TO      WK-HEN-DATE3.
     MOVE      WK-HEN-DATE      TO      WK-F07.
*    取引先ＣＤ
     MOVE      SHO-F01          TO      WK-F08.
*    改行コード
     MOVE      X"0D0A"          TO      WK-F09.
*****DISPLAY "ABAB" UPON CONS.
*
 TENPO-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫出力先ファイル判定→ファイル出力            *
****************************************************************
 TBLSET-SEC            SECTION.
*
     MOVE     "TBLSET-SEC"   TO      S-NAME.
*送信用伝票データ出力
     MOVE     SPACE          TO      D01-REC.
     INITIALIZE                      D01-REC.
     MOVE     WK-REC         TO      D01-REC.
*#2023/10/23 NAV STＪＡＮＣＤの７桁目が空白の時対象外とする
     IF  WK-F05(7:1) = SPACE
         GO                  TO      TBLSET-EXIT
     END-IF.
*#2023/10/23 NAV ED
     WRITE    D01-REC.
     ADD      1              TO      WK-RTCNTT.
*
 TBLSET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫別検品取引先設定マスタ                      *
****************************************************************
 SOKKENF-READ-SEC      SECTION.
*
     MOVE     "SOKKENF-READ-SEC" TO      S-NAME.
*
     READ      SOKKENF   AT  END
               MOVE     "END"    TO      END-FLG
               NOT  AT  END
               DISPLAY "TORICD = " SOK-F02 UPON CONS
     END-READ.
*
 SOKKENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗マスタ読込み                                *
****************************************************************
 HSHOTBL-READ-SEC       SECTION.
*
     MOVE     "HSHOTBL-READ-SEC"  TO      S-NAME.
*
     READ      HSHOTBL    AT  END
               MOVE     "END"    TO      TBL-FLG
               GO                TO      HSHOTBL-READ-EXIT
     END-READ.
*    取引先判定
*#2030/07/31 NAV ST
***  IF        SHO-F04  NOT =  SOK-F01
*        IF        SHO-F01  NOT =  SOK-F02
*              IF  SHO-F04  =  "TK"
*              AND SHO-F01  =  2363
*                  DISPLAY "BBB"  UPON  CONS
***                 CONTINUE
*              ELSE
*                   MOVE     "END"    TO      TBL-FLG
*              END-IF
*        END-IF
***  END-IF.
     IF        SHO-F04  =  SOK-F01
     AND       SHO-F01  =  SOK-F02
               CONTINUE
     ELSE
               IF  SOK-F01  =  "TK"
               AND SOK-F02  =  2363
                   IF  SHO-F01 NOT =  2363
                       MOVE     "END"    TO      TBL-FLG
                   END-IF
               ELSE
                   MOVE     "END"    TO      TBL-FLG
               END-IF
     END-IF.
*#2030/07/31 NAV ED
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*           リスト出力処理                          3.1.1      *
****************************************************************
 LISTWT-SEC   SECTION.
*
 LISTWT-010.
     MOVE      WK-FLCDT       TO       FILECD.
     MOVE      WK-RTNMT       TO       ROUTENM.
     MOVE      WK-RTCDT       TO       ROUTECD.
     MOVE      WK-RTCNTT      TO       DATASU.
*
     MOVE      SPACE          TO       K01-REC.
     INITIALIZE                        K01-REC.
     MOVE      WK-RTCNTT      TO       K01-F01.
     MOVE      X"0D0A"        TO       K01-F02.
     WRITE K01-REC.
*
 LISTWT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     MOVE     "HMEIMS-READ-SEC"   TO      S-NAME.
*
     READ      HMEIMS     INVALID
               MOVE     "INV"    TO      HMEIMS-INV-FLG
               NOT  INVALID
               MOVE     SPACE   TO      HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＳＵＢ商品名称マスタ読込
****************************************************************
 SUBMEIL1-READ-SEC      SECTION.
*
     MOVE     "SUBMEIL1-READ-SEC" TO      S-NAME.
*
     READ      SUBMEIL1   INVALID
               MOVE     "INV"    TO     SUBMEIL1-INV-FLG
               NOT  INVALID
               MOVE     SPACE   TO      SUBMEIL1-INV-FLG
     END-READ.
*
 SUBMEIL1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
