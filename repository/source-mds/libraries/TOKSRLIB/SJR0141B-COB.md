# SJR0141B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0141B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　　　　　　　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　　　　　*
*    モジュール名　　　　：　受領データ共通変換                *
*                            　ジュンテンドー(受領・返品）
*    作成日　　　　　　　：　2017/09/01 INOUE                  *
*    処理概要　　　　　　：　各社毎の受領返品データを、　　　  *
*                            共通データに変換する。　　　　　　*
*                                                              *
*    更新日　　　　　　　：　    /  /                          *
*    更新内容　　　　　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0141B.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2017/09/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領データファイル
     SELECT   JUNJYRL1  ASSIGN    TO        DA-01-VI-JUNJYRL1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    ONL-STATUS
                        RECORD    KEY       ONL-F04
                                            ONL-F07
                                            ONL-F03
                                            ONL-F15
                        ORGANIZATION   IS   INDEXED.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
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
*受領受信状況ファイル
     SELECT   COMJYOF   ASSIGN    TO        DA-01-VI-COMJYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                                            JYO-F03   JYO-F04
                        FILE STATUS    IS   JYO-STATUS.
*受領返品累積ファイル
     SELECT   COMRUIF   ASSIGN    TO        DA-01-VI-COMRUIL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       RUI-F03   RUI-F08
                                            RUI-F04   RUI-F31
                        FILE  STATUS   IS   RUI-STATUS.
*伝票区分変換マスタ
     SELECT   DENHENF   ASSIGN    TO        DA-01-VI-DENHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HEN-F01 HEN-F02
                        FILE  STATUS   IS   HEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領データ　
******************************************************************
 FD  JUNJYRL1           LABEL RECORD   IS   STANDARD.
     COPY     JUNJYRL1  OF    XFDLIB
              JOINING   ONL   PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF    XFDLIB
              JOINING   TEN   PREFIX.
******************************************************************
*    得意先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF    XFDLIB
              JOINING   TOK   PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF    XFDLIB
              JOINING   TBL   PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF    XFDLIB
              JOINING   MEI   PREFIX.
******************************************************************
*    受領受信状況ファイル
******************************************************************
 FD  COMJYOF            LABEL RECORD   IS   STANDARD.
     COPY     COMJYOF   OF    XFDLIB
              JOINING   JYO   PREFIX.
******************************************************************
*    受領返品累積ファイル
******************************************************************
 FD  COMRUIF            LABEL RECORD   IS   STANDARD.
     COPY     COMRUIF   OF    XFDLIB
              JOINING   RUI   PREFIX.
******************************************************************
*    伝票区分変換マスタマスタ
******************************************************************
 FD  DENHENF            LABEL RECORD   IS   STANDARD.
     COPY     DENHENF   OF    XFDLIB
              JOINING   HEN   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  RWRT-CNT                PIC  9(08)     VALUE  ZERO.
*
*伝票番号ブレイク判定用
 01  WK-DENNO                PIC  X(08)     VALUE "00000000".
*取引先ブレイク判定用
 01  WK-TORI                 PIC  9(08)     VALUE  ZERO.
*エラーチェック用
 01  WK-ERR-KEIJOUBI         PIC  X(01)     VALUE  SPACE.
 01  WK-ERR-GENTANKA         PIC  X(01)     VALUE  SPACE.
*カウント
 01  WK-CNT-HEN              PIC  9(07)     VALUE  ZERO.
 01  WK-CNT-JYR              PIC  9(07)     VALUE  ZERO.
 01  WK-DEN-HEN              PIC  9(07)     VALUE  ZERO.
 01  WK-DEN-JYR              PIC  9(07)     VALUE  ZERO.
*
 01  STR-TIME                PIC  9(04)     VALUE  ZERO.
*
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  COMJYOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  COMRUIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DENHENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  ONL-F03NUMERIC.
     03  ONL-F03NUM         PIC  9(08).
*
*伝票ヘッダーレコード（１）←当ＰＧでは非使用
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(02).
     03  WK-DEPB02          PIC  9(05).
     03  WK-DEPB03          PIC  9(03).
     03  WK-DEPB04          PIC  X(02).
     03  WK-DEPB05          PIC  9(08).
     03  WK-DEPB06          PIC  9(09).
     03  WK-DEPB07          PIC  9(08).
     03  WK-DEPB08          PIC  9(08).
     03  WK-DEPB09          PIC  9(08).
     03  WK-DEPB10          PIC  9(08).
     03  WK-DEPB11          PIC  9(08).
     03  WK-DEPB12          PIC  X(28).
     03  WK-DEPB13          PIC  9(01).
     03  WK-DEPB14          PIC  9(12).
     03  WK-DEPB15          PIC  9(09).
     03  WK-DEPB16          PIC  X(119).
     03  WK-DEPB17          PIC  9(11).
     03  WK-DEPB18          PIC  9(01).
     03  WK-DEPB19          PIC  9(06).

*伝票ヘッダレコード（２）←当ＰＧでは非使用
 01  WK-DEPC-REC.
     03  WK-DEPC01          PIC  X(01).
     03  WK-DEPC02          PIC  X(02).
     03  WK-DEPC03          PIC  X(01).
     03  WK-DEPC04          PIC  X(30).
     03  WK-DEPC05          PIC  X(20).
     03  WK-DEPC06          PIC  X(20).
     03  WK-DEPC07          PIC  X(17).
     03  WK-DEPC08          PIC  X(17).
     03  WK-DEPC09          PIC  X(12).
     03  WK-DEPC10          PIC  X(01).
     03  WK-DEPC11          PIC  X(04).
     03  WK-DEPC12          PIC  X(03).
*伝票明細レコード←当ＰＧでは非使用
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(02).
     03  WK-DEPD02          PIC  9(08).
     03  WK-DEPD03          PIC  9(02).
     03  WK-DEPD04          PIC  9(01).
     03  WK-DEPD05          PIC  X(13).
     03  WK-DEPD06          PIC  9(04).
     03  WK-DEPD07          PIC  9(08).
     03  WK-DEPD08          PIC  9(05).
     03  WK-DEPD09          PIC  9(08)V99.
     03  WK-DEPD10          PIC  9(09).
     03  WK-DEPD11          PIC  9(08)V99.
     03  WK-DEPD12          PIC  9(09).
     03  WK-DEPD13          PIC  9(05).
     03  WK-DEPD14          PIC  9(08)V99.
     03  WK-DEPD15          PIC  9(09).
     03  WK-DEPD16          PIC  X(100).
     03  WK-DEPD17          PIC  X(33).
     03  WK-DEPD18          PIC  9(11).
     03  WK-DEPD19          PIC  9(01).
     03  WK-DEPD20          PIC  9(06).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  ONL-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  RUI-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0141B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0141B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0141B".
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
     03  MSG-RWT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " UPDATE= ".
         05  RWT-CNT        PIC   9(06).
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORI              PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORI.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JUNJYRL1.
     MOVE      "JUNJYRL1"   TO   AB-FILE.
     MOVE      ONL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
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
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
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
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMJYOF.
     MOVE      "COMJYOL1"   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRUIF.
     MOVE      "COMRUIL2"   TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DENHENF.
     MOVE      "DENHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
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
     OPEN     INPUT     JUNJYRL1  HMEIMS
                        HTENMS    HSHOTBL
                        HTOKMS    DENHENF.
     OPEN     I-O       COMRUIF.
     OPEN     I-O       COMJYOF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    MOVE     SPACE     TO        WK-HEAD-REC.
*    INITIALIZE                   WK-HEAD-REC.
*    MOVE     SPACE     TO        WK-MEISAI-REC.
*    INITIALIZE                   WK-MEISAI-REC.
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
     MOVE      WK-TIME(1:4)     TO     STR-TIME.
*データ初期READ
 INIT-01.
     READ     JUNJYRL1
              AT END    MOVE      9         TO  END-FG
              DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
              GO              TO    INIT-EXIT
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*　  伝票区分変換マスタチェック
     MOVE     PARA-TORI      TO        HEN-F01.
     MOVE     ONL-F04        TO        HEN-F02.
     PERFORM  DENHEN-READ-SEC.
     IF       DENHENF-INV-FLG  NOT =  "INV"
         IF     HEN-F05  =   "2"
                GO       TO     INIT-01
         END-IF
     ELSE
                GO       TO     INIT-01
     END-IF.
*    返品
       IF   ( ONL-F04   =    "21" )
*             レコード件数カウントアップ
              ADD       1            TO    WK-CNT-HEN
*             伝票枚数カウントアップ
              IF   ONL-F03           NOT = WK-DENNO
                   ADD  1            TO    WK-DEN-HEN
                   MOVE ONL-F03      TO    WK-DENNO
              END-IF
       END-IF.
*    受領
       IF   ( ONL-F04   NOT = "21" )
*             レコード件数カウントアップ
              ADD       1            TO    WK-CNT-JYR
*             伝票枚数カウントアップ
              IF   ONL-F03           NOT = WK-DENNO
                   ADD   1           TO    WK-DEN-JYR
                   MOVE ONL-F03      TO    WK-DENNO
              END-IF
       END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*              メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-01.
     PERFORM  EDIT-SEC.
*
 MAIN-02.
     READ     JUNJYRL1
                AT END
                           MOVE      9         TO  END-FG
                           GO                  TO  MAIN-EXIT
                NOT AT END
                           ADD       1         TO  RD-CNT
     END-READ.
*　  伝票区分変換マスタチェック
     MOVE     PARA-TORI      TO        HEN-F01.
     MOVE     ONL-F04        TO        HEN-F02.
     PERFORM  DENHEN-READ-SEC.
     IF       DENHENF-INV-FLG  NOT =  "INV"
         IF     HEN-F05  =   "2"
                GO       TO     MAIN-02
         END-IF
     ELSE
                GO       TO     MAIN-02
     END-IF.
*   返品伝票カウント
*
     IF   ( ONL-F04       =    "21" )
*           レコード件数カウントアップ
            ADD       1            TO    WK-CNT-HEN
*           伝票枚数カウントアップ
            IF   ONL-F03           NOT = WK-DENNO
*                同一伝票_存在チェック
                 PERFORM   COMRUIF-START-SEC
                 IF        COMRUIF-INV-FLG  =  "   "
                           PERFORM   COMRUIF-READ-SEC2
                           MOVE  ONL-F03   TO   ONL-F03NUMERIC
*                  同一伝票_存在時は伝票枚数カウントなし
                           IF  ( RUI-F03 = PARA-TORI  ) AND
                               ( RUI-F08 = ONL-F10    ) AND
                               ( RUI-F04 = ONL-F03NUM )
*                              ( RUI-F31 = ONL-F15    )
*T↓
                                 CONTINUE
*                           DISPLAY "DATE=" ONL-F10   UPON CONS
*                           DISPLAY "DEN =" ONL-F03   UPON CONS
*T↑
*T↑
                           ELSE
                                 ADD  1         TO    WK-DEN-HEN
                                 MOVE ONL-F03   TO    WK-DENNO
                           END-IF
                 ELSE
                           ADD  1            TO    WK-DEN-HEN
                           MOVE ONL-F03      TO    WK-DENNO
                 END-IF
            END-IF
     END-IF.
*
*   受領伝票カウント
*
     IF   ( ONL-F04   NOT =    "21" )
*           レコード件数カウントアップ
            ADD       1            TO    WK-CNT-JYR
*           伝票枚数カウントアップ
            IF   ONL-F03           NOT = WK-DENNO
*                同一伝票_存在チェック
                 PERFORM   COMRUIF-START-SEC
                 IF        COMRUIF-INV-FLG  =  "   "
                           PERFORM   COMRUIF-READ-SEC2
                           MOVE  ONL-F03   TO   ONL-F03NUMERIC
*                  同一伝票_存在時は伝票枚数カウントなし
                           IF  ( RUI-F03 = PARA-TORI  ) AND
                               ( RUI-F08 = ONL-F10    ) AND
                               ( RUI-F04 = ONL-F03NUM )
*                              ( RUI-F31 = ONL-F15    )
*T↓
                                 CONTINUE
*                                DISPLAY "DATE=" ONL-F10 UPON CONS
*                                DISPLAY "DEN =" ONL-F03 UPON CONS
*T↑
                           ELSE
                                 ADD  1         TO    WK-DEN-JYR
                                 MOVE ONL-F03   TO    WK-DENNO
                           END-IF
                 ELSE
                           ADD  1            TO    WK-DEN-JYR
                           MOVE ONL-F03      TO    WK-DENNO
                 END-IF
            END-IF
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*          　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*
     MOVE     SPACE         TO        RUI-REC.
     INITIALIZE                       RUI-REC.
*
*存在チェック
     MOVE     PARA-TORI     TO        RUI-F03.
     MOVE     ONL-F10       TO        RUI-F08.
*    MOVE     PARA-JDATE    TO        RUI-F01.
*    MOVE     PARA-JTIME    TO        RUI-F02.
     MOVE     ONL-F03       TO        ONL-F03NUMERIC
     MOVE     ONL-F03NUM    TO        RUI-F04.
     MOVE     ONL-F15       TO        RUI-F31.
     PERFORM  COMRUIF-READ-SEC.
*
*    計上日(IN:8桁)
     IF  ONL-F10     NUMERIC
         MOVE        "2"            TO    LINK-IN-KBN
         MOVE        ONL-F10        TO    LINK-IN-YMD8
         CALL        "SKYDTCKB"  USING    LINK-IN-KBN
                                          LINK-IN-YMD6
                                          LINK-IN-YMD8
                                          LINK-OUT-RET
                                          LINK-OUT-YMD8
         IF          LINK-OUT-RET   =     ZERO
             MOVE    ONL-F10        TO    RUI-F08
             MOVE    " "            TO    WK-ERR-KEIJOUBI
         ELSE
             MOVE    ONL-F10        TO    RUI-F08
             MOVE    "1"            TO    WK-ERR-KEIJOUBI
         END-IF
     ELSE
         MOVE        ZERO           TO    RUI-F08
         MOVE        "1"            TO    WK-ERR-KEIJOUBI
     END-IF.
*取引先マスタ索引
     MOVE     PARA-TORI      TO        TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
*店舗マスタ索引
     MOVE     PARA-TORI      TO        TEN-F52.
     MOVE     ONL-F07        TO        TEN-F011.
     PERFORM  HTENMS-READ-SEC.
*商品変換ＴＢＬ索引
     MOVE     PARA-TORI      TO        TBL-F01.
     MOVE     ONL-F23        TO        TBL-F02.
     PERFORM  HSHOTBL-READ-SEC.
*原単価
     IF     ( ONL-F20   NOT NUMERIC ) OR
            ( ONL-F20   =   ZERO    )
              MOVE   "1"     TO        WK-ERR-GENTANKA
              MOVE   ZERO    TO        RUI-F35
     ELSE
              MOVE   " "     TO        WK-ERR-GENTANKA
     END-IF.
*バッチ日付
     MOVE     PARA-JDATE     TO        RUI-F01.
*バッチ時刻
     MOVE     PARA-JTIME     TO        RUI-F02.
*バッチ取引先
     MOVE     PARA-TORI      TO        RUI-F03.
*伝票番号
     MOVE     ONL-F03        TO        RUI-F04.
*元伝票番号
*    F05 : INIT
*相手伝票区分
     MOVE     ONL-F04        TO        RUI-F06.
*店舗コード
     MOVE     ONL-F07        TO        RUI-F07.
*計上日
*分類コード
*    F09 : INIT
*訂正区分（Ｈ）
*    F10 : INIT
*直接納品先コード
*    F11 : INIT
*コメント
*    F12 : INIT
*行番号
     MOVE     ONL-F15        TO        RUI-F31.
*赤黒区分
     IF       ONL-F19        <         0
              MOVE     1     TO        RUI-F32
     END-IF.
*相手商品コード
     MOVE     ONL-F23        TO        RUI-F33.
*数量
     MOVE     ONL-F19        TO        RUI-F34.
*    COMPUTE  RUI-F34   =    ONL-F325  /   10.
*原単価
     IF       WK-ERR-GENTANKA  NOT =  "1"
              MOVE ONL-F20   TO        RUI-F35
*             COMPUTE   RUI-F35    =   ONL-F315  /  100
     END-IF.
*原価金額
*    MOVE     ONL-F15        TO        RUI-F36.
     COMPUTE  RUI-F36   =    RUI-F34   *   RUI-F35.
*商品名カナ１
*    MOVE     ONL-F16(1:15)    TO      RUI-F37.
*商品名カナ２
*    MOVE     ONL-F16(21:10)   TO      RUI-F38.
*返品理由
     MOVE     ONL-F22          TO      RUI-F39
*訂正区分（Ｍ）
*    F40 : INIT
*売単価
     MOVE     ONL-F21          TO      RUI-F41.
*    COMPUTE  RUI-F41  =  ONL-F317  /  100.
*自社伝票区分
     MOVE     HEN-F04        TO        RUI-F51.
*
*サカタ商品コード
     IF   HSHOTBL-INV-FLG    NOT =    "INV"
          MOVE    TBL-F031   TO        RUI-F52  MEI-F011
          MOVE    TBL-F0321  TO        RUI-F53  MEI-F0121
          MOVE    TBL-F0322  TO        RUI-F54  MEI-F0122
          MOVE    TBL-F0323  TO        RUI-F55  MEI-F0123
*商品名カナ１・２
          PERFORM HMEIMS-READ-SEC
          IF      HMEIMS-INV-FLG       NOT =    "INV"
                  MOVE    MEI-F031     TO       RUI-F37
                  MOVE    MEI-F032     TO       RUI-F38
          END-IF
     END-IF.
*
*    エラー区分１（取引先マスタ）
     IF   HTOKMS-INV-FLG  =  "INV"
          MOVE  "1"          TO        RUI-F561
     END-IF.
*    エラー区分２（店舗マスタ）
     IF   HTENMS-INV-FLG  =  "INV"
          MOVE  "1"          TO        RUI-F562
     END-IF.
*    エラー区分３（計上日）
     IF   WK-ERR-KEIJOUBI  = "1"
          MOVE  "1"          TO        RUI-F563
     END-IF.
*    エラー区分４（原単価）
     IF   WK-ERR-GENTANKA  = "1"
          MOVE  "1"          TO        RUI-F564
     END-IF.
*    エラー区分５（商品変換ＴＢＬ）
     IF   HSHOTBL-INV-FLG  = "INV"
          MOVE  "1"          TO        RUI-F565
     END-IF.
*    エラー区分６（非使用）
*    F566 : INIT
*    エラー区分７（非使用）
*    F567 : INIT
*    エラー区分８（非使用）
*    F568 : INIT
*    エラー区分９（非使用）
*    F569 : INIT
*    エラー区分10（非使用）
*    F56A : INIT
*    返品累積データ作成区分
*    F57  : INIT
*    返品累積データ作成日
*    F58  : INIT
*  存在時：REWRITE
     IF       COMRUIF-INV-FLG  =  SPACE
              REWRITE  RUI-REC
              ADD    1                TO        RWRT-CNT
              GO                      TO        EDIT-EXIT
     END-IF.
*
*受領返品累積ファイル出力
     WRITE  RUI-REC.
     ADD    1              TO   WRT-CNT .
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        WRT-CNT   >   ZERO
*              受領受信状況ファイル出力
               PERFORM   COMJYOF-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     MOVE      RWRT-CNT  TO      RWT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-RWT   UPON CONS.
     DISPLAY   NC"うち受領＝" WK-CNT-JYR NC"件" UPON CONS.
     DISPLAY   NC"　　　　　" WK-DEN-JYR NC"枚" UPON CONS.
     DISPLAY   NC"うち返品＝" WK-CNT-HEN NC"件" UPON CONS.
     DISPLAY   NC"　　　　　" WK-DEN-HEN NC"枚" UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     JUNJYRL1  HTENMS   HSHOTBL  HMEIMS
               HTOKMS    COMJYOF  COMRUIF  DENHENF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　受領受信状況ファイル出力
****************************************************************
 COMJYOF-WRT-SEC         SECTION.
*
     MOVE   "COMJYOF-WRT-SEC"   TO   S-NAME.
*
     ACCEPT  WK-TIME       FROM      TIME.
*
 COMJYOF-WRT-HEN.
* 返品
     IF      WK-CNT-HEN =  ZERO
             GO            TO        COMJYOF-WRT-JYR
     END-IF.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
     MOVE    PARA-TORI     TO        JYO-F03.
     MOVE    02            TO        JYO-F04.
     READ    COMJYOF
       INVALID
         CONTINUE
       NOT INVALID
         DELETE   COMJYOF
     END-READ.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
*日付
     MOVE    PARA-JDATE    TO        JYO-F01.
*時刻
     MOVE    PARA-JTIME    TO        JYO-F02.
*取引先
     MOVE    PARA-TORI     TO        JYO-F03.
*データ種別
     MOVE    02            TO        JYO-F04.
*受信開始時刻（非使用）
*    INIT
*受信終了時刻（非使用）
*    INIT
*データ変換開始時刻
     MOVE    STR-TIME      TO        JYO-F07.
*データ変換終了時刻
     MOVE    WK-TIME(1:4)  TO        JYO-F08.
*受信件数・伝票枚数
     MOVE    WK-CNT-HEN    TO        JYO-F09.
     MOVE    WK-DEN-HEN    TO        JYO-F10.
*受信状況フラグ
*    INIT
*変換状況フラグ
*    INIT
*更新状況フラグ
*    INIT
*実行完了ＦＬＧ
*    INIT
*結果ＦＬＧ
*    INIT
*
     WRITE   JYO-REC.
*
 COMJYOF-WRT-JYR.
* 受領
     IF      WK-CNT-JYR =  ZERO
             GO            TO        COMJYOF-WRT-EXIT
     END-IF.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
     MOVE    PARA-TORI     TO        JYO-F03.
     MOVE    01            TO        JYO-F04.
     READ    COMJYOF
       INVALID
         CONTINUE
       NOT INVALID
         DELETE   COMJYOF
     END-READ.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
*日付
     MOVE    PARA-JDATE    TO        JYO-F01.
*時刻
     MOVE    PARA-JTIME    TO        JYO-F02.
*取引先
     MOVE    PARA-TORI     TO        JYO-F03.
*データ種別
     MOVE    01            TO        JYO-F04.
*受信開始時刻（非使用）
*    INIT
*受信終了時刻（非使用）
*    INIT
*データ変換開始時刻
     MOVE    STR-TIME      TO        JYO-F07.
*データ変換終了時刻
     MOVE    WK-TIME(1:4)  TO        JYO-F08.
*受信件数・伝票枚数
     MOVE    WK-CNT-JYR    TO        JYO-F09.
     MOVE    WK-DEN-JYR    TO        JYO-F10.
*受信状況フラグ
*    INIT
*変換状況フラグ
*    INIT
*更新状況フラグ
*    INIT
*実行完了ＦＬＧ
*    INIT
*結果ＦＬＧ
*    INIT
*
     WRITE   JYO-REC.
*
     GO      TO   COMJYOF-WRT-EXIT.
*
 COMJYOF-WRT-EXIT.
     EXIT.
****************************************************************
*　　取引先マスタ読込
****************************************************************
 HTOKMS-READ-SEC         SECTION.
*
     MOVE   "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ  HTOKMS
           INVALID     MOVE  "INV"  TO  HTOKMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
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
****************************************************************
*　　受領返品累積ファイル検索
****************************************************************
 COMRUIF-READ-SEC        SECTION.
*
     MOVE   "COMRUIF-READ-SEC"  TO   S-NAME.
*
     READ  COMRUIF
           INVALID     MOVE  "INV"  TO  COMRUIF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  COMRUIF-INV-FLG
     END-READ.
*
 COMRUIF-READ-EXIT.
     EXIT.
****************************************************************
*　　受領返品累積ファイルSTART
****************************************************************
 COMRUIF-START-SEC       SECTION.
*
     MOVE   "COMRUIF-START-SEC"  TO   S-NAME.
*
     MOVE      PARA-TORI     TO    RUI-F03
     MOVE      ONL-F10       TO    RUI-F08
     MOVE      ONL-F03       TO    ONL-F03NUMERIC
     MOVE      ONL-F03NUM    TO    RUI-F04
     MOVE      ZERO          TO    RUI-F31
     START     COMRUIF  KEY IS >=  RUI-F03
                                   RUI-F08
                                   RUI-F04
                                   RUI-F31
        INVALID
               MOVE  "INV"   TO    COMRUIF-INV-FLG
        NOT INVALID
               MOVE  SPACE   TO    COMRUIF-INV-FLG
     END-START.
*
 COMRUIF-START-EXIT.
     EXIT.
****************************************************************
*　　受領返品累積ファイル検索(SEQ-READ)
****************************************************************
 COMRUIF-READ-SEC2       SECTION.
*
     MOVE   "COMRUIF-READ-SEC2"  TO   S-NAME.
*
     READ  COMRUIF
           NEXT AT END     GO    TO    COMRUIF-READ-EXIT2
     END-READ.
*
 COMRUIF-READ-EXIT2.
     EXIT.
****************************************************************
*　　伝票区分変換マスタ読込
****************************************************************
 DENHEN-READ-SEC         SECTION.
*
     MOVE   "DENHEN-READ-SEC"   TO   S-NAME.
*
     READ  DENHENF
           INVALID     MOVE  "INV"  TO  DENHENF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  DENHENF-INV-FLG
     END-READ.
*
 DENHEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
