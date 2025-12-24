# SBM1060L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM1060L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳ　　　　　　　　　*
*    業務名　　　　　　　：　イオン流通ＢＭＳ                  *
*    モジュール名　　　　：　イオン取引先振分未登録リスト      *
*    作成日／更新日　　　：　12/12/06                          *
*    作成者／更新者　　　：　ＮＡＶ三浦                        *
*    処理概要　　　　　　：　発注メッセージよりイオン取引先振分*
*                            マスタ未登録の店舗を出力します。　*
*                            　　　                            *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM1060L.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/12/06.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   YA
     YB           IS   YB
     YA-22        IS   YA-22
     YB-21        IS   YB-21
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*イオン取引先付替エラーワーク
     SELECT  IONTERWK     ASSIGN    TO     DA-01-S-IONTERWK
                          ACCESS    MODE   SEQUENTIAL
                          FILE      STATUS ION-ST.
*取引先マスタ
     SELECT  HTOKMS       ASSIGN    TO     DA-01-VI-TOKMS2
                          ORGANIZATION     INDEXED
                          ACCESS    MODE   RANDOM
                          RECORD    KEY    TOK-F01
                          FILE      STATUS TOK-ST.
*プリント定義ファイル
     SELECT  PRTFILE      ASSIGN    TO     LP-04-PRTF
                          FILE      STATUS PRT-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = イオン取引先付替エラーワーク                       *
****************************************************************
 FD  IONTERWK
                       BLOCK     CONTAINS  81   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      IONTERWK  OF   XFDLIB
                       JOINING   ION       AS   PREFIX.
****************************************************************
*  FILE= 取引先マスタ                                          *
****************************************************************
 FD  HTOKMS            BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
                       LABEL     RECORD    IS   OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  ION-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  ION-ERR           PIC N(15) VALUE
         NC"イオン取引先付替エラーワーク".
     03  TOK-ERR           PIC N(15) VALUE
         NC"取引先マスタエラー".
     03  PRT-ERR           PIC N(15) VALUE
         NC"プリントファイルエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  END-FLG           PIC X(03) VALUE     SPACE.
     03  P-CNT             PIC 9(05) VALUE     ZERO.
     03  L-CNT             PIC 9(05) VALUE     ZERO.
     03  SYSDATE           PIC 9(06) VALUE     ZERO.
     03  IX                PIC 9(02) VALUE     ZERO.
     03  HTOKMS-INV-FLG    PIC X(03) VALUE     SPACE.
     03  LINK-ERRKBN       PIC X(01) VALUE     SPACE.
***  日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1        PIC  9(02).
     03  WK-DATE8-YY2        PIC  9(06).
 01  WK-DATE8-R         REDEFINES  WK-DATE8.
     03  WK-YYYY             PIC  9(04).
     03  WK-MM               PIC  9(02).
     03  WK-DD               PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  SYS-TIME2               PIC  9(08).
 01  FILLER            REDEFINES  SYS-TIME2.
     03  SYS-TIMEW           PIC  9(06).
     03  FILLER              PIC  9(02).
 01  WK-SYS-TIME             PIC  9(08)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*ヘッダ１
 01  HD1.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-01                  PIC  X(08).
     03  FILLER                  PIC  X(18)  VALUE  SPACE.
     03  FILLER                  PIC  N(20)  VALUE
         NC"＜イオン取引先振分マスタ未登録リスト＞"
                                 CHARACTER  TYPE  IS  YA-22.
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  YA.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  YA.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  YA.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  YA.
*ヘッダ２
 01  HD2.
     03  FILLER                  PIC  X(116) VALUE  SPACE.
     03  HD2-01                  PIC  99.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"："
                                 CHARACTER   TYPE   IS  YA.
     03  HD2-02                  PIC  99.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"："
                                 CHARACTER   TYPE   IS  YA.
     03  HD2-03                  PIC  99.
*ヘッダ３
 01  HD3.
     03  FILLER                  PIC  X(43)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"受信バッチＮＯ："
                                 CHARACTER   TYPE   IS  YB-21.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-01                  PIC  X(10).
     03  FILLER                  PIC  X(03)  VALUE  " - ".
     03  HD3-02                  PIC  X(05).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-03                  PIC  9(08).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD3-04                  PIC  N(15)
                                 CHARACTER   TYPE   IS  YA.
*ヘッダ４
*01  HD4.
*    03  FILLER                  PIC  X(52)  VALUE  SPACE.
*    03  FILLER                  PIC  N(05)  VALUE
*                                NC"ＭＳＧ種："
*                                CHARACTER   TYPE   IS  YB-21.
*    03  HD4-01                  PIC  N(10)
*                                CHARACTER   TYPE   IS  YA.
*ヘッダ５
 01  HD5                         CHARACTER   TYPE   IS  YB-21.
     03  FILLER                  PIC  X(14)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"受信取引先".
     03  FILLER                  PIC  X(30)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"会社ＣＤ".
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"店舗ＣＤ".
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"付替取引先".
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"ＭＳＧ種".
*線＝
 01  SEN1.
     03  FILLER                  PIC  X(136) VALUE  ALL "=".
*線－
 01  SEN2.
     03  FILLER                  PIC  X(136) VALUE  ALL "-".
*明細
 01  DT1                         CHARACTER  TYPE  IS  YA.
     03  FILLER                  PIC  X(15)  VALUE  SPACE.
     03  DT1-01                  PIC  9(08).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  N(15).
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-03                  PIC  X(13).
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT1-04                  PIC  9(05).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  DT1-05                  PIC  9(08).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  DT1-06                  PIC  N(04).
*------------------------------------------------------------*
*LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
*01  LINK-JDATE            PIC 9(08).
*01  LINK-JTIME            PIC 9(08).
*01  LINK-TRICD            PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 ION-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE IONTERWK.
     MOVE        ION-ST    TO        E-ST.
     MOVE       "IONTERWK" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     ION-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃イオン取引先付替エラーワーク異常＃" UPON CONS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE       "TOKMS2  " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃イオン取引先未登録リスト　異常＃" UPON CONS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     MOVE        PRT-ST    TO        E-ST.
     MOVE        "PRTFILE " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PRT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃イオン取引先未登録リスト　異常＃" UPON CONS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     DISPLAY NC"＃取引先別受信件数リスト　開始＃" UPON CONS.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM   END-SEC.
     DISPLAY NC"＃取引先別受信件数リスト　終了＃" UPON CONS.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT  IONTERWK HTOKMS.
     OPEN      OUTPUT PRTFILE.
*
*システム日付・時刻の取得
     ACCEPT   SYSDATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSDATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-DATE8.
*    起動時の時刻で更新する。
     ACCEPT   SYS-TIME    FROM  TIME.
     MOVE     SYS-TIME            TO   WK-SYS-TIME.
*行カウンターを初期セット
     MOVE     99          TO      L-CNT.
*イオン取引先振分エラーワーク初期リード
     PERFORM  IONTERWK-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*行カウンターチェック
     IF    L-CNT  >  61
           PERFORM  HEAD-WT-SEC
     END-IF.
*明細セット
*    受信取引先ＣＤ
     MOVE  ION-F03           TO   DT1-01.
*    受信取引先名取得
     MOVE  ION-F03           TO   TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
*
     IF  HTOKMS-INV-FLG = "INV"
         MOVE  ALL NC"＊"    TO   DT1-02
     ELSE
         MOVE  TOK-F02       TO   DT1-02
     END-IF.
*    会社ＣＤ
     MOVE  ION-F04           TO   DT1-03.
*    店舗ＣＤ
     MOVE  ION-F05           TO   DT1-04.
*    付替後取引先ＣＤ
     MOVE  ION-F06           TO   DT1-05.
*    ＭＳＧ種判定
     EVALUATE  ION-F07
         WHEN  "1"
         MOVE NC"発注　　"      TO   DT1-06
         WHEN  "2"
         MOVE NC"受領　　"      TO   DT1-06
         WHEN  "3"
         MOVE NC"受領訂正"      TO   DT1-06
         WHEN  "4"
         MOVE NC"返品　　"      TO   DT1-06
         WHEN  "5"
         MOVE NC"支払　　"      TO   DT1-06
         WHEN  OTHER
         MOVE ALL NC"＊"        TO   DT1-06
     END-EVALUATE.
 MAIN010.
*明細部出力
     WRITE    PRT-REC   FROM      DT1       AFTER  1.
     WRITE    PRT-REC   FROM      SEN2      AFTER  2.
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC   AFTER  1.
*    行カウンター
     ADD      4                       TO   L-CNT.
*次レコード読み込み
     PERFORM  IONTERWK-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    イオン取引先振分エラーワーク読込                  ALL     *
****************************************************************
 IONTERWK-READ-SEC      SECTION.
     MOVE "IONTERWK-READ-SEC" TO   S-NAME.
*
     READ  IONTERWK AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   IONTERWK-READ-EXIT
     END-READ.
*パラメタデータ区分チェック
*    IF    TWK-F01  NOT =  LINK-KBN
*          MOVE   "END"      TO   END-FLG
*    END-IF.
*
 IONTERWK-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ読込                                ALL       *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE "HTOKMS-READ-SEC"  TO   S-NAME.
*
     READ  HTOKMS
           INVALID      MOVE  "INV"   TO   HTOKMS-INV-FLG
           NOT  INVALID MOVE  SPACE   TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    ヘッダ印字処理                                  ALL       *
****************************************************************
 HEAD-WT-SEC           SECTION.
     MOVE "HEAD-WT-SEC"      TO   S-NAME.
*改頁チェック
     IF  P-CNT  >  ZERO
         MOVE   SPACE        TO   PRT-REC
         WRITE  PRT-REC      AFTER   PAGE
     END-IF.
*ヘッダを出力する。
*    ＰＧＩＤ
     MOVE      "SBM1060L"             TO   HD1-01.
*    システム日付
     MOVE      WK-YYYY                TO   HD1-02.
     MOVE      WK-MM                  TO   HD1-03.
     MOVE      WK-DD                  TO   HD1-04.
*    頁
     ADD       1                      TO   P-CNT.
     MOVE      P-CNT                  TO   HD1-05.
*    時刻
     MOVE      SYS-HH                 TO   HD2-01.
     MOVE      SYS-MN                 TO   HD2-02.
     MOVE      SYS-SS                 TO   HD2-03.
*    受信バッチ番号（日付）
     MOVE      ION-F01(1:4)           TO   HD3-01(1:4).
     MOVE      ION-F01(5:2)           TO   HD3-01(6:2).
     MOVE      ION-F01(7:2)           TO   HD3-01(9:2).
     MOVE      "/"                    TO   HD3-01(5:1).
     MOVE      "/"                    TO   HD3-01(8:1).
*    受信バッチ番号（時刻）
     MOVE      ION-F02(1:2)           TO   HD3-02(1:2).
     MOVE      ION-F02(3:2)           TO   HD3-02(4:2).
     MOVE      ":"                    TO   HD3-02(3:1).
 MAIN040.
*ヘッダ部出力
     WRITE    PRT-REC   FROM      HD1       AFTER  3.
     WRITE    PRT-REC   FROM      HD2       AFTER  1.
     WRITE    PRT-REC   FROM      HD3       AFTER  1.
*    WRITE    PRT-REC   FROM      HD4       AFTER  2.
     WRITE    PRT-REC   FROM      SEN1      AFTER  3.
     WRITE    PRT-REC   FROM      HD5       AFTER  1.
     WRITE    PRT-REC   FROM      SEN1      AFTER  1.
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC   AFTER  1.
*行カウンター
     MOVE     14                  TO        L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     IONTERWK.
     CLOSE     HTOKMS.
     CLOSE     PRTFILE.
     DISPLAY NC"＃出力頁　＝　" P-CNT  "　＃"  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SBM1030L   END PROGRAM  >>******************

```
