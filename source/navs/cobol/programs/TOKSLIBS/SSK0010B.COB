****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レスシステム　　　　　*
*    業務名　　　　　　　：　オンライン業務　　　　　　　　　　*
*    モジュール名　　　　：　受領データ累積処理　　　　　　　　*
*    作成日／更新日　　　：　2014/03/17                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　ケーヨーより受信した受領データを *
*                            読み、各マスタを参照しながら、受　*
*                            領累積データを作成する。          *
*    作成日／更新日　　　：　2014/04/22                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　受領累積データの存在チェック追加　*
*                            納品予定日が数字以外の対応　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSK0010B.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          2014/03/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    ONL-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*売上伝票ファイル
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F23
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYF   ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*受領受信状況データ
     SELECT   KEIJYOF   ASSIGN    TO        DA-01-VI-KEIJYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                                            JYO-F03   JYO-F04
                        FILE STATUS    IS   JYO-STATUS.
*受領累積データ
     SELECT   KEIJYRF   ASSIGN    TO        DA-01-VI-KEIJYRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYR-F01   JYR-F02
                                            JYR-F03   JYR-F04
                                            JYR-F05   JYR-F06
                                            JYR-F08   JYR-F10
                        FILE  STATUS   IS   JYR-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２０４８  ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  ONL-REC.
     03  ONL-01A                PIC  X(01).
     03  ONL-01B                PIC  X(127).
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYF            LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    受領受信状況データ
******************************************************************
 FD  KEIJYOF            LABEL RECORD   IS   STANDARD.
     COPY     KEIJYOF   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    受領累積データ
******************************************************************
 FD  KEIJYRF            LABEL RECORD   IS   STANDARD.
     COPY     KEIJYRF   OF        XFDLIB
              JOINING   JYR       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  SHTDENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
*##2014/04/22 NAV ST 追加
 01  KEIJYRF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*##2014/04/22 NAV ED
*ヘッダレコード退避ワーク
 01  WK-HEAD-REC.
     03  HD-F01             PIC  X(01).
     03  HD-F02             PIC  9(03).
     03  HD-F03             PIC  9(04).
     03  HD-F04             PIC  9(06).
     03  HD-F05             PIC  9(06).
     03  HD-FIL             PIC  X(108).
*明細レコード退避ワーク
 01  WK-MEISAI-REC.
     03  MS-F01             PIC  X(01).
     03  MS-F02             PIC  9(02).
     03  MS-F03             PIC  X(13).
     03  MS-F04             PIC  9(06).
     03  MS-F05             PIC  X(01).
     03  MS-F06             PIC  9(06).
     03  MS-F07             PIC  9(09).
     03  MS-F08             PIC  9(09).
     03  MS-F09             PIC  9(09).
     03  MS-F10             PIC  9(02).
     03  MS-F11             PIC  X(02).
     03  MS-FIL             PIC  X(68).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  ONL-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  JYR-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0010B".
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
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      ONL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL5"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "TENMS1"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMDAYF.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYOF.
     MOVE      "KEIJYOL1"   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYRF.
     MOVE      "KEIJYRL1"   TO   AB-FILE.
     MOVE      JYR-STATUS   TO   AB-STS.
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     CVCSG001
                        HTENMS    HSHOTBL
                        HMEIMS    SHTDENF.
     OPEN     I-O       KEIJYRF.
     OPEN     I-O       KEIJYOF   JSMDAYF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-HEAD-REC.
     INITIALIZE                   WK-HEAD-REC.
     MOVE     SPACE     TO        WK-MEISAI-REC.
     INITIALIZE                   WK-MEISAI-REC.
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
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*ヘッダ処理
     IF    ONL-01A  =  "B"
           MOVE      SPACE         TO    WK-HEAD-REC
           INITIALIZE                    WK-HEAD-REC
           MOVE      ONL-REC       TO    WK-HEAD-REC
     END-IF.
*明細行
     IF    ONL-01A  =  "C"
           MOVE      ONL-REC       TO    WK-MEISAI-REC
           PERFORM                       EDIT-SEC
     END-IF
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*
     MOVE     SPACE         TO        JYR-REC.
     INITIALIZE                       JYR-REC.
*##2014/04/22 NAV ST 納品予定日が数字以外の場合、対応を追加
*    納品予定日（数字以外の場合、０をセットする。）
     IF  HD-F04  NUMERIC
         MOVE     "3"           TO        LINK-IN-KBN
         MOVE     HD-F04        TO        LINK-IN-YMD6
         CALL     "SKYDTCKB"    USING     LINK-IN-KBN
                                          LINK-IN-YMD6
                                          LINK-IN-YMD8
                                          LINK-OUT-RET
                                          LINK-OUT-YMD8
         IF          LINK-OUT-RET   =     ZERO
             MOVE    LINK-OUT-YMD8  TO    JYR-F09
         ELSE
             MOVE    ZERO           TO    JYR-F09
         END-IF
     ELSE
         MOVE        ZERO           TO    JYR-F09
     END-IF.
*##2014/04/22 NAV ED
*    検収日
     MOVE     "3"           TO        LINK-IN-KBN.
     MOVE     HD-F05        TO        LINK-IN-YMD6.
     CALL     "SKYDTCKB"    USING     LINK-IN-KBN
                                      LINK-IN-YMD6
                                      LINK-IN-YMD8
                                      LINK-OUT-RET
                                      LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =     ZERO
         MOVE    LINK-OUT-YMD8  TO    JYR-F10
     ELSE
         MOVE    ZERO           TO    JYR-F10
     END-IF.
*納品予定日＝０の場合、検収日をセットする。
     IF  JYR-F09 = ZERO
         MOVE    JYR-F10        TO    JYR-F09
     END-IF.
*売上伝票ファイル索引
     MOVE     173           TO        DEN-F01.
     MOVE     MS-F09        TO        DEN-F23.
     MOVE     0             TO        DEN-F04.
     IF  MS-F11  =  "07"
         MOVE "41"          TO        DEN-F051
     ELSE
         MOVE "40"          TO        DEN-F051
     END-IF.
     MOVE    HD-F02         TO        DEN-F07.
     MOVE    JYR-F09        TO        DEN-F112.
     MOVE    MS-F10         TO        DEN-F03.
     PERFORM  SHTDENF-READ-SEC.
*店舗マスタ索引
     MOVE     173           TO        TEN-F52.
     MOVE     HD-F02        TO        TEN-F011.
     PERFORM  HTENMS-READ-SEC.
*商品変換ＴＢＬ索引
     MOVE     173           TO        TBL-F01.
     MOVE     MS-F03        TO        TBL-F02.
     PERFORM  HSHOTBL-READ-SEC.
*商品名称マスタ索引
     IF   SHTDENF-INV-FLG = "INV"
          IF   HSHOTBL-INV-FLG = SPACE
               MOVE   TBL-F031   TO        MEI-F011
               MOVE   TBL-F032   TO        MEI-F012
               PERFORM  HMEIMS-READ-SEC
          END-IF
     ELSE
          IF   HSHOTBL-INV-FLG = SPACE
               MOVE   TBL-F031   TO        MEI-F011
               MOVE   TBL-F032   TO        MEI-F012
               PERFORM  HMEIMS-READ-SEC
          END-IF
     END-IF.
*受領累積データ項目セット
*    受信日
     MOVE     PARA-JDATE    TO        JYR-F01.
*    受信時刻
     MOVE     PARA-JTIME    TO        JYR-F02.
*    取引先ＣＤ
     MOVE     173           TO        JYR-F03.
*    店舗ＣＤ
     MOVE     HD-F02        TO        JYR-F04.
*    伝票番号
     MOVE     MS-F09        TO        JYR-F05.
*    行番号
     MOVE     MS-F10        TO        JYR-F06.
*    伝票区分（ケーヨー）
     MOVE     MS-F11        TO        JYR-F07  WK-DENKU.
*    伝票区分（サカタ）
     IF  MS-F11  =  "07"
         MOVE "41"          TO        JYR-F08
     ELSE
         MOVE "40"          TO        JYR-F08
     END-IF.
*    ＪＡＮＣＤ
     MOVE     MS-F03        TO        JYR-F11.
*    検品数量
     MOVE     MS-F06        TO        JYR-F12.
*****IF       MS-F05  =  "-"
*             COMPUTE  JYR-F12  =  JYR-F12  *  -1
*****END-IF.
     COMPUTE  JYR-F12  =  JYR-F12  /  10.
*    原価単価
     MOVE     MS-F07        TO        JYR-F13.
     COMPUTE  JYR-F13  =  JYR-F13  /  100.
*    原価金額
     MOVE     MS-F08        TO        JYR-F14.
     COMPUTE  JYR-F14  =  JYR-F14  /  100.
*****IF       MS-F05  =  "-"
*             COMPUTE  JYR-F14  =  JYR-F14  *  -1
*****END-IF.
*    ケーヨー取引先ＣＤ
     MOVE     HD-F03        TO        JYR-F15.
*    サカタ商品ＣＤ＋品単ＣＤ
     IF  SHTDENF-INV-FLG  =  SPACE
         MOVE DEN-F1411     TO        JYR-F16
         MOVE DEN-F1412(1:5) TO       JYR-F17
         MOVE DEN-F1412(6:2) TO       JYR-F18
         MOVE DEN-F1412(8:1) TO       JYR-F19
     ELSE
         IF  HSHOTBL-INV-FLG  =  SPACE
             MOVE TBL-F031   TO       JYR-F16
             MOVE TBL-F0321  TO       JYR-F17
             MOVE TBL-F0322  TO       JYR-F18
             MOVE TBL-F0323  TO       JYR-F19
         END-IF
     END-IF.
*    商品名
     IF  SHTDENF-INV-FLG  =  SPACE
         MOVE DEN-F1421      TO       JYR-F20
         MOVE DEN-F1422      TO       JYR-F21
     ELSE
         IF  HSHOTBL-INV-FLG  =  SPACE
         AND HMEIMS-INV-FLG   =  SPACE
             MOVE MEI-F031   TO       JYR-F20
             MOVE MEI-F032   TO       JYR-F21
         END-IF
     END-IF.
*    売上伝票Ｆ項目取得
*    項目（受注数、出荷数、発注日、納品日、出荷日、出荷場所
*    ルート、振分場所、バッチ日付、時刻、売価単価、売価金額）
     IF   SHTDENF-INV-FLG  =  SPACE
          MOVE  DEN-F50     TO        JYR-F22
          MOVE  DEN-F15     TO        JYR-F23
          MOVE  DEN-F111    TO        JYR-F24
          MOVE  DEN-F112    TO        JYR-F25
          MOVE  DEN-F113    TO        JYR-F26
          MOVE  DEN-F08     TO        JYR-F27
          MOVE  DEN-F42     TO        JYR-F28
          MOVE  DEN-F48     TO        JYR-F29
          MOVE  DEN-F46     TO        JYR-F30
          MOVE  DEN-F47     TO        JYR-F31
          MOVE  DEN-F173    TO        JYR-F32
          MOVE  DEN-F182    TO        JYR-F33
     END-IF.
*    エラー区分１（店舗マスタ）
     IF   HTENMS-INV-FLG  =  "INV"
          MOVE  "1"         TO        JYR-F83
     END-IF.
*    エラー区分２（売上伝票Ｆ）
     IF   SHTDENF-INV-FLG  =  "INV"
          MOVE  "1"         TO        JYR-F84
     ELSE
          MOVE  "1"         TO        JYR-F93
     END-IF.
*    エラー区分３（商品変換ＴＢＬ）
     IF   HSHOTBL-INV-FLG  =  "INV"
          MOVE  "1"         TO        JYR-F85
     END-IF.
*    エラー区分４（商品名称マスタ）
     IF   HMEIMS-INV-FLG  =  "INV"
          MOVE  "1"         TO        JYR-F86
     END-IF.
*    登録日付
     MOVE     SYS-DATEW     TO        JYR-F96.
*    登録時刻
     MOVE     WK-TIME(1:6)  TO        JYR-F97.
*    登録部門
     MOVE     "2920"        TO        JYR-F98.
*    登録担当者
     MOVE     "99"          TO        JYR-F99.
*##2014/04/22 NAV ST データ存在チェックを追加
*    受領累積データが存在しない場合、出力する。
     PERFORM  KEIJYRF-READ-SEC.
     IF  KEIJYRF-INV-FLG = "INV"
         WRITE  JYR-REC
*        受領受信状況データ出力
         PERFORM KEIJYOF-WRT-SEC
         ADD      1             TO   WRT-CNT
     END-IF.
*##2014/04/22 NAV ED
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　受領受信状況データ出力
****************************************************************
 KEIJYOF-WRT-SEC         SECTION.
*
     MOVE   "KEIJYOF-WRT-SEC"   TO   S-NAME.
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
     MOVE    173           TO        JYO-F03.
     MOVE    WK-DENKU      TO        JYO-F04.
     READ    KEIJYOF
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   KEIJYOF-010
     END-READ.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
     MOVE    173           TO        JYO-F03.
     MOVE    WK-DENKU      TO        JYO-F04.
*    伝票区分（サカタ）
     IF  MS-F11  =  "07"
         MOVE "41"         TO        JYO-F05
     ELSE
         MOVE "40"         TO        JYO-F05
     END-IF.
     ADD     1             TO        JYO-F06.
     MOVE    SYS-DATEW     TO        JYO-F98.
     MOVE    WK-TIME(1:6)  TO        JYO-F99.
     WRITE   JYO-REC.
     GO      TO   KEIJYOF-WRT-EXIT.
*
 KEIJYOF-010.
*
     ADD     1             TO        JYO-F06.
     REWRITE JYO-REC.
*
 KEIJYOF-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        WRT-CNT   >   ZERO
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYF-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     CVCSG001  SHTDENF  HTENMS  HSHOTBL
               HMEIMS    JSMDAYF  KEIJYOF KEIJYRF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　当日スケジュールマスタ出力
****************************************************************
 JSMDAYF-WRT-SEC         SECTION.
*
     MOVE   "JSMDAYF-WRT-SEC"   TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    173           TO        TJS-F03.
     READ    JSMDAYF
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYF-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    173           TO        TJS-F03.
     MOVE    02            TO        TJS-F04.
     MOVE    WRT-CNT       TO        TJS-F09.
     WRITE   TJS-REC.
     GO      TO    JSMDAYF-WRT-EXIT.
*
 JSMDAYF-010.
*
     MOVE    WRT-CNT       TO        TJS-F09.
     REWRITE TJS-REC.
*
 JSMDAYF-WRT-EXIT.
     EXIT.
****************************************************************
*　　売上伝票ファイル読込
****************************************************************
 SHTDENF-READ-SEC        SECTION.
*
     MOVE   "SHTDENF-READ-SEC"  TO   S-NAME.
*
     READ  SHTDENF
           INVALID     MOVE  "INV"  TO  SHTDENF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　店舗マスタ読込
****************************************************************
 HTENMS-READ-SEC         SECTION.
*
     MOVE   "HTENMS-READ-SEC"   TO   S-NAME.
*
     READ  HTENMS
           INVALID     MOVE  "INV"  TO  HTENMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　商品変換ＴＢＬ読込
****************************************************************
 HSHOTBL-READ-SEC        SECTION.
*
     MOVE   "HSHOTBL-READ-SEC"  TO   S-NAME.
*
     READ  HSHOTBL
           INVALID     MOVE  "INV"  TO  HSHOTBL-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC         SECTION.
*
     MOVE   "HMEIMS-READ-SEC"   TO   S-NAME.
*
     READ  HMEIMS
           INVALID     MOVE  "INV"  TO  HMEIMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*##2014/04/22 NAV ST ↓以下を追加
****************************************************************
*　　受領累積データ読込
****************************************************************
 KEIJYRF-READ-SEC        SECTION.
*
     MOVE   "KEIJYRF-READ-SEC"  TO   S-NAME.
*
     READ  KEIJYRF
           INVALID     MOVE  "INV"  TO  KEIJYRF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  KEIJYRF-INV-FLG
     END-READ.
*
 KEIJYRF-READ-EXIT.
     EXIT.
*##2014/04/22 NAV ST ↑追加
*-------------< PROGRAM END >------------------------------------*
