***********************************************************
*                                                         *
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：受領データチェック　　　           *
*    作成日／更新日　：2010/10/13                         *
*    作成者／更新者　：ＮＡＶ阿部                         *
*    処理概要　　　　：                                   *
*      受領累積ファイルと基本情報ファイルをマッチングし   *
*      受領アンマッチデータを作成する。                   *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3780B.
 AUTHOR.               Y.A.
 DATE-WRITTEN.         2010/10/13.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受領累積データ
     SELECT   NFJYURF   ASSIGN    TO        DA-01-VI-NFJYURL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JYU-F01   JYU-F05
                                            JYU-F07   JYU-F08

                        FILE  STATUS   IS   JYU-STATUS.
*基本情報ファイル
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL7
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JHO-F06   JHO-F07
                                            JHO-F08

                        FILE  STATUS   IS   JHO-STATUS.
*受領アンマッチデータ　
     SELECT   NFUNMAF            ASSIGN    TO   DA-01-VI-NFUNMAL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    UNM-F01  UNM-F02
                                                UNM-F03
                                 STATUS         UNM-STATUS.
*
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受領累積ファイル  レコード長＝ 200 BYTE
****************************************************************
 FD  NFJYURF            LABEL RECORD   IS   STANDARD.
     COPY     NFJYURF   OF        XFDLIB
              JOINING   JYU       PREFIX.

******************************************************************
*    基本情報ファイル
******************************************************************
 FD  NFJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JHO       PREFIX.
******************************************************************
*    受領アンマッチデータ
******************************************************************
 FD  NFUNMAF            LABEL RECORD   IS   STANDARD.
     COPY     NFUNMAF   OF        XFDLIB
              JOINING   UNM       PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JYU-STATUS         PIC  X(02).
     03  JHO-STATUS         PIC  X(02).
     03  UNM-STATUS         PIC  X(02).
*
 01  FLG-AREA.
     03  END-FLG            PIC  X(03).
     03  END-JHO            PIC  X(03).
*エラー区分領域
 01  ERR-FLG.
     03  ERR-FLG1           PIC  X(01).
     03  ERR-FLG2           PIC  X(01).
     03  ERR-FLG3           PIC  X(01).
     03  ERR-FLG4           PIC  X(01).
*ワーク領域
 01  WK-AREA.
     03  WK-SURYO           PIC  9(09).
     03  WK-JYUF13          PIC  9(09).
     03  WK-JHOME10         PIC  9(09).
     03  WK-JYUF14          PIC  9(09).
     03  WK-JHOME12         PIC  9(09).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  JYU-ERR            PIC N(15) VALUE
         NC"受領累積データエラー".
     03  JHO-ERR            PIC N(15) VALUE
         NC"基本情報ファイルエラー".
     03  UNM-ERR            PIC N(15) VALUE
         NC"受領アンマッチＦエラー".
*読込フラグ領域
 01  FLG-AREA.
     03  FG-END             PIC  X(03)  VALUE SPACE.
     03  FG-INF-END         PIC  X(03)  VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  IN-CNT             PIC  9(07)  VALUE ZERO.
     03  OT-CNT             PIC  9(07)  VALUE ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME             PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE             PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST               PIC  9(02).
*日付変換サブルーチン用ワーク
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*------------------------------------------------------------*
 LINKAGE              SECTION.
 01  PARA-JDATE             PIC   9(08).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-JDATE.
**************************************************************
 DECLARATIVES.
 JYU-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJYURF.
     MOVE        JYU-STATUS  TO      E-ST.
     MOVE        "NFJYURF"   TO      E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYU-ERR   UPON      CONS.
     MOVE        "4000"      TO      PROGRAM-STATUS.
     STOP        RUN.
 JHO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF.
     MOVE        JHO-STATUS  TO      E-ST.
     MOVE        "NFJOHOF"   TO      E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JHO-ERR   UPON      CONS.
     MOVE        "4000"      TO      PROGRAM-STATUS.
     STOP        RUN.
 UNM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFUNMAF.
     MOVE        UNM-STATUS  TO      E-ST.
     MOVE        "NFUNMAF"   TO      E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JHO-ERR   UPON      CONS.
     MOVE        "4000"      TO      PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
************* UNTIL  FG-END = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
* ファイルのＯＰＥＮ
     OPEN  INPUT  NFJYURF.
     OPEN  INPUT  NFJOHOF.
     OPEN  OUTPUT NFUNMAF.
* 受領累積ファイル読み込み位置設定
     MOVE  SPACE            TO  JYU-REC.
     INITIALIZE                 JYU-REC.
     MOVE  PARA-JDATE       TO  JYU-F01.
     START  NFJYURF   KEY  >=   JYU-F01 JYU-F05 JYU-F07 JYU-F08
            INVALID   KEY
            MOVE     "END"      TO   END-FLG
     END-START
*
     IF     END-FLG   =   "END"
            DISPLAY NC"＃対象データ無し＃" UPON CONS
            GO TO  INIT-EXIT
     ELSE
            PERFORM   NFJYURF-RD-SEC
            IF   END-FLG   =   "END"
                 DISPLAY NC"＃対象データ無し＃" UPON CONS
                 GO TO  INIT-EXIT
            END-IF
     END-IF
*基本情報ファイル読み込み位置設定
     MOVE   SPACE               TO   JHO-REC.
     INITIALIZE                      JHO-REC.
*
     MOVE   JYU-F05             TO   JHO-F06
     MOVE   JYU-F07             TO   JHO-F07
     MOVE   JYU-F08             TO   JHO-F08
*
     START  NFJOHOF   KEY  >=   JHO-F06  JHO-F07  JHO-F08
            INVALID   KEY
            MOVE     "END"      TO   END-JHO
     END-START
*
     IF     END-JHO   =   "END"
         DISPLAY NC"＃基本情報対象データなし＃" UPON CONS
         MOVE   HIGH-VALUE      TO   JHO-REC
     ELSE
            PERFORM   NFJOHOF-RD-SEC
            IF   END-JHO   =   "END"
               DISPLAY NC"＃基本情報対象データなし＃" UPON CONS
               MOVE   HIGH-VALUE      TO   JHO-REC
            END-IF
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*  入力ファイル読込み処理                            1.1       *
****************************************************************
 NFJYURF-RD-SEC          SECTION.
     READ  NFJYURF
       AT  END
         MOVE  "END"        TO  END-FLG
     END-READ.
*
     IF   END-FLG     >    "END"
          MOVE   HIGH-VALUE   TO  JYU-REC
          GO TO  NFJYURF-RD-EXIT
     END-IF.
*    DISPLAY "END-FLG = " END-FLG  UPON CONS.
*    IF   JYU-REC =  HIGH-VALUE
*         DISPLAY  " NFUNMAF READ SEC"  UPON CONS
*    END-IF.
*
     IF   PARA-JDATE     >    0
          IF   JYU-F01   >    PARA-JDATE
               MOVE  "END"        TO  END-FLG
               GO TO  NFJYURF-RD-EXIT
          END-IF
     END-IF.
*
     IF   JYU-F04   NOT =  ZERO
          MOVE  "END"        TO  END-FLG
          GO TO  NFJYURF-RD-EXIT
     END-IF.
*
     ADD  1   TO  IN-CNT.
*
 NFJYURF-RD-EXIT.
     EXIT.
****************************************************************
*  基本情報ファイル読み込み                          1.1       *
****************************************************************
 NFJOHOF-RD-SEC         SECTION.
     READ  NFJOHOF
       AT  END
         MOVE  "END"        TO  FG-INF-END
         GO TO  NFJOHOF-RD-EXIT
     END-READ.

     ADD  1   TO  IN-CNT.

 NFJOHOF-RD-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     MOVE  SPACE            TO  ERR-FLG.

 MAIN-001.
***  DISPLAY "AAAA"        UPON      CONS.
     IF   JYU-F05   <   JHO-F06  OR
          JYU-F07   <   JHO-F07  OR
          JYU-F08   <   JHO-F08
          PERFORM   NFUNMAF-WT-SEC
          PERFORM   NFJYURF-RD-SEC
          GO   TO   MAIN-EXIT
     END-IF.
*
  MAIN-002.
***  DISPLAY "BBBB"        UPON      CONS.
     IF   JYU-F05   >   JHO-F06  OR
          JYU-F07   >   JHO-F07  OR
          JYU-F08   >   JHO-F08
          PERFORM   NFJOHOF-RD-SEC
          GO   TO   MAIN-EXIT
     END-IF.
*
  MAIN-003.
***  DISPLAY "CCCC"        UPON      CONS.
     IF   JYU-F05   =   JHO-F06  AND
          JYU-F07   =   JHO-F07  AND
          JYU-F08   =   JHO-F08
          MOVE    SPACE      TO   ERR-FLG
          PERFORM   NFJYURF-CHK-SEC
          IF  ERR-FLG1 NOT =  SPACE   OR
              ERR-FLG2 NOT =  SPACE   OR
              ERR-FLG3 NOT =  SPACE   OR
              ERR-FLG4 NOT =  SPACE
              PERFORM   NFUNMAF-WT-SEC
          END-IF
          PERFORM   NFJYURF-RD-SEC
          PERFORM   NFJOHOF-RD-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*  受領データチェック処理                            2.1
****************************************************************
 NFJYURF-CHK-SEC       SECTION.
     MOVE  "NFJYURF-CHK-SEC" TO  S-NAME.


     IF    JYU-F10   NOT =   JHO-F13
           MOVE   "1"       TO  ERR-FLG1
     END-IF.
*
     IF    JYU-F11   NOT =   JHO-F14
           MOVE   "1"       TO  ERR-FLG1
     END-IF.
*
     COMPUTE   WK-SURYO  =   JYU-F12  /  10.
     IF   JYU-F15   =  02
          IF    WK-SURYO  NOT =   JHO-F20
                MOVE   "1"       TO  ERR-FLG2
          END-IF
     ELSE
          IF    WK-SURYO  NOT =   JHO-F19
                MOVE   "1"       TO  ERR-FLG2
          END-IF
     END-IF.
*
     MOVE  JYU-F13         TO    WK-JYUF13.
     MOVE  JHO-ME10        TO    WK-JHOME10.
*
     IF   WK-JYUF13  NOT =  WK-JHOME10
          MOVE   "1"       TO  ERR-FLG3
     END-IF.
*
     MOVE  JYU-F14         TO    WK-JYUF14.
     MOVE  JHO-ME12        TO    WK-JHOME12.
*
     IF   WK-JYUF14  NOT =  WK-JHOME12
          MOVE   "1"       TO  ERR-FLG4
     END-IF.
*
 CH-FNJYURDT-EXIT.
     EXIT.
****************************************************************
*  受領アンマッチデータ出力                          2.1
****************************************************************
 NFUNMAF-WT-SEC         SECTION.
     MOVE  "NFUNMAF-WT-SEC" TO  S-NAME.

     MOVE  SPACE            TO  UNM-REC.
     INITIALIZE                 UNM-REC.

     MOVE  JYU-F05          TO  UNM-F01.
     MOVE  JYU-F07          TO  UNM-F02.
     MOVE  JYU-F08          TO  UNM-F03.
     MOVE  JYU-F09          TO  UNM-F04.
*
     IF    JYU-F15     =    02
           COMPUTE  WK-SURYO     =  JHO-F20  *  10
     ELSE
           COMPUTE  WK-SURYO     =  JHO-F19  *  10
     END-IF.
     MOVE  WK-SURYO         TO  UNM-F05.
*
     MOVE  JHO-ME10         TO  UNM-F06.
     MOVE  JHO-ME12         TO  UNM-F07.
     MOVE  JYU-F12          TO  UNM-F08.
     MOVE  JYU-F13          TO  UNM-F09.
     MOVE  JYU-F14          TO  UNM-F10.
     MOVE  JHO-F27          TO  UNM-F11.
     MOVE  JYU-F02          TO  UNM-F12.
     MOVE  ERR-FLG1         TO  UNM-F13.
     MOVE  ERR-FLG2         TO  UNM-F14.
     MOVE  ERR-FLG3         TO  UNM-F15.
     MOVE  ERR-FLG4         TO  UNM-F16.
*
     WRITE  UNM-REC.
     ADD  1  TO  OT-CNT.
 EDWT-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.
*
     DISPLAY  "NFJYURT IN = "  IN-CNT  UPON CONS.
     DISPLAY  "NFUNMAF OT = "  OT-CNT  UPON CONS.
*
* ファイルのＣＬＯＳＥ
     CLOSE  NFJYURF  NFJOHOF  NFUNMAF.
*
 END-EXIT.
     EXIT.
*****************<<  SSY3780B   END PROGRAM  >>******************
