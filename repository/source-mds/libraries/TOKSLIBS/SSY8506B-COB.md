# SSY8506B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8506B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　くろがねやオンラインシステム　　　*
*    業務名　　　　　　　：　くろがねやオンラインシステム　　　*
*    モジュール名　　　　：　出荷送信データ作成２              *
*    作成日／更新日　　　：　2006/12/11                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　くろがねや送信データを作成する。　*
*                            送信データ作成２                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8506B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/12/11.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*くろがねや出荷確定データ
     SELECT   SNDKGSS   ASSIGN    TO        DA-01-S-SNDKGSS
                        FILE      STATUS    SND-STATUS.
*くろがねや送信情報データ
     SELECT   KGHENKF   ASSIGN    TO        DA-01-VI-KGHENKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEN-F01   HEN-F02
                                            HEN-F03   HEN-F04
                                            HEN-F05
                        FILE      STATUS    HEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    くろがねや送信データ
******************************************************************
 FD  SNDKGSS
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  SND-REC.
     03  SND-DT                  PIC  X(128).
*
******************************************************************
*    くろがねや送信情報データ
******************************************************************
 FD  KGHENKF            LABEL RECORD   IS   STANDARD.
     COPY     KGHENKF   OF        XFDLIB
              JOINING   HEN       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ファイルヘッダ編集
 01  WK-FIL-REC.
     03  WK-FIL-F01          PIC  X(01).
     03  WK-FIL-F02          PIC  9(02).
     03  WK-FIL-F03          PIC  9(08).
     03  WK-FIL-F04          PIC  9(06).
     03  WK-FIL-F05          PIC  9(08).
     03  WK-FIL-F06          PIC  9(06).
     03  WK-FIL-F07          PIC  X(02).
     03  WK-FIL-F08          PIC  9(06).
     03  WK-FIL-F09          PIC  9(02).
     03  WK-FIL-F10          PIC  X(08).
     03  WK-FIL-F11          PIC  9(03).
     03  WK-FIL-F12          PIC  9(06).
     03  WK-FIL-F13          PIC  9(05).
     03  WK-FIL-F14          PIC  X(65).
*明細ヘッダ編集
 01  WK-HED-REC.
     03  WK-HED-F01          PIC  X(01).
     03  WK-HED-F02          PIC  9(02).
     03  WK-HED-F03          PIC  9(05).
     03  WK-HED-F04          PIC  9(04).
     03  WK-HED-F05          PIC  9(03).
     03  WK-HED-F06          PIC  9(01).
     03  WK-HED-F07          PIC  9(02).
     03  WK-HED-F08          PIC  9(08).
     03  WK-HED-F09          PIC  9(06).
     03  WK-HED-F10          PIC  9(02).
     03  WK-HED-F11          PIC  X(06).
     03  WK-HED-F12          PIC  X(01).
     03  WK-HED-F13          PIC  X(06).
     03  WK-HED-F14          PIC  X(10).
     03  WK-HED-F15          PIC  X(07).
     03  WK-HED-F16          PIC  X(10).
     03  WK-HED-F17          PIC  X(02).
     03  WK-HED-F18          PIC  9(03).
     03  WK-HED-F19          PIC  X(49).
*明細編集
 01  WK-MEI-REC.
     03  WK-MEI-F01          PIC  X(01).
     03  WK-MEI-F02          PIC  9(02).
     03  WK-MEI-F03          PIC  9(06).
     03  WK-MEI-F04          PIC  9(02).
     03  WK-MEI-F05          PIC  9(02).
     03  WK-MEI-F06          PIC  9(08).
     03  WK-MEI-F07          PIC  9(08).
     03  WK-MEI-F08          PIC  9(07).
     03  WK-MEI-F09          PIC  X(13).
     03  WK-MEI-F10          PIC  9(01).
     03  WK-MEI-F11          PIC  X(20).
     03  WK-MEI-F12          PIC  X(20).
     03  WK-MEI-F13          PIC  9(09).
     03  WK-MEI-F14          PIC  9(07).
     03  WK-MEI-F15          PIC  9(06).
     03  WK-MEI-F16          PIC  9(06).
     03  WK-MEI-F17          PIC  9(06).
     03  WK-MEI-F18          PIC  X(04).
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SND-CNT             PIC  9(08)     VALUE  ZERO.
     03  IDX                 PIC  9(01)     VALUE  ZERO.
 01  WK-KEY-JYOHO.
     03  WK-HEN-F01          PIC  9(08)     VALUE  ZERO.
     03  WK-HEN-F02          PIC  9(05)     VALUE  ZERO.
     03  WK-HEN-F04          PIC  9(09)     VALUE  ZERO.
     03  WK-HEN-F096         PIC  9(06)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*システム時刻の編集
     03  SYS-TIME.
         05  SYS-TIME1     PIC 9(06).
         05  SYS-TIME2     PIC 9(02).
 01  WK-ST.
     03  SND-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8506B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8506B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8506B".
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
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDKGSS.
     MOVE      "SNDKGSS "   TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KGHENKF.
     MOVE      "KGHENKL1"   TO   AB-FILE.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KGHENKF.
     OPEN     OUTPUT    SNDKGSS.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
*
*システム時間編集*
     ACCEPT      SYS-TIME  FROM      TIME.
*システム日付編集*
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
*    くろがねや送信情報データ読込み
     PERFORM KGHENKF-READ-SEC.
*    終了条件判定
     IF       END-FLG  NOT =  "END"
              MOVE     HEN-F01    TO   WK-HEN-F01
              MOVE     HEN-F02    TO   WK-HEN-F02
**************MOVE     HEN-F096   TO   WK-HEN-F096
              MOVE      SPACE     TO   SND-REC
              INITIALIZE               SND-REC
              PERFORM  FILE-HEAD-SEC
              MOVE     WK-FIL-REC  TO  SND-DT
              WRITE    SND-REC
              ADD      1           TO SND-CNT
              MOVE     SPACE       TO SND-REC
              INITIALIZE              SND-REC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    発注日／店舗ＣＤがブレイクした場合
*****IF       HEN-F01   NOT =  WK-HEN-F01
*    OR       HEN-F02   NOT =  WK-HEN-F02
*****OR       HEN-F096  NOT =  WK-HEN-F096
*             IF   IDX  >  ZERO
*                      WRITE     SND-REC
*             END-IF
*             ADD       1         TO   SND-CNT
*             MOVE     HEN-F01    TO   WK-HEN-F01
*             MOVE     HEN-F02    TO   WK-HEN-F02
**************MOVE     HEN-F04    TO   WK-HEN-F04
**************MOVE     HEN-F096   TO   WK-HEN-F096
*             MOVE     ZERO       TO   IDX
*             MOVE      SPACE     TO   SND-REC
*             INITIALIZE               SND-REC
*    END-IF.
*    テーブルカウントアップ
     ADD      1                   TO   IDX.
*    レコード区分判定
     EVALUATE HEN-F03
         WHEN "1"
***           PERFORM  FILE-HEAD-SEC
***           MOVE     WK-FIL-REC  TO  SND-DT
              GO                  TO   MAIN010
         WHEN "2"
              IF       HEN-F04  NOT =  WK-HEN-F04
                       PERFORM  MEISAI-HEAD-SEC
                       MOVE WK-HED-REC TO SND-DT
                       WRITE  SND-REC
                       ADD    1        TO SND-CNT
                       MOVE   SPACE    TO SND-REC
                       INITIALIZE         SND-REC
                       MOVE HEN-F04    TO WK-HEN-F04
                       PERFORM  MEISAI-BODY-SEC
                       MOVE WK-MEI-REC TO SND-DT
              ELSE
                       PERFORM  MEISAI-BODY-SEC
                       MOVE WK-MEI-REC TO SND-DT
              END-IF
     END-EVALUATE.
*    テーブル判定
     WRITE     SND-REC.
     ADD       1         TO   SND-CNT.
     MOVE      ZERO      TO   IDX.
     MOVE      SPACE     TO   SND-REC.
     INITIALIZE               SND-REC.
 MAIN010.
*    くろがねや送信情報データ読込み
     PERFORM  KGHENKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　カーマ送信情報データ読込み　　　　　　　　　　　　　　　　*
****************************************************************
 KGHENKF-READ-SEC    SECTION.
*
     READ     KGHENKF
              AT  END
                  MOVE     "END"    TO  END-FLG
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 KMSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　ファイルヘッダ作成
****************************************************************
 FILE-HEAD-SEC         SECTION.
*
     MOVE     "FILE-HEAD-SEC"      TO  S-NAME.
*
     MOVE      SPACE               TO  WK-FIL-REC.
     INITIALIZE                        WK-FIL-REC.
*    レコード区分
     MOVE      "A"                 TO  WK-FIL-F01.
*    データ種別
*****MOVE      "51"                TO  WK-FIL-F02.
     EVALUATE  HEN-F092
         WHEN   22   MOVE   "62"   TO  WK-FIL-F02
         WHEN   23   MOVE   "63"   TO  WK-FIL-F02
         WHEN   24   MOVE   "64"   TO  WK-FIL-F02
         WHEN   25   MOVE   "65"   TO  WK-FIL-F02
         WHEN   26   MOVE   "66"   TO  WK-FIL-F02
         WHEN  OTHER MOVE   "62"   TO  WK-FIL-F02
     END-EVALUATE.
*    データ処理日
     MOVE      SYS-DATEW           TO  WK-FIL-F03.
*    データ処理時刻
     MOVE      SYS-TIME1           TO  WK-FIL-F04.
*    データ日付
     MOVE      HEN-F01             TO  WK-FIL-F05.
*    センターコード
     MOVE      ZERO                TO  WK-FIL-F06.
*    予備
     MOVE      ZERO                TO  WK-FIL-F07.
*    最終送信先コード
     MOVE      ZERO                TO  WK-FIL-F08.
*    ステーションアドレス
     MOVE      ZERO                TO  WK-FIL-F09.
*    直接送信宛先
     MOVE      SPACE               TO  WK-FIL-F10.
*    レコードサイズ
     MOVE      256                 TO  WK-FIL-F11.
*    レコード件数
     MOVE      HEN-F06             TO  WK-FIL-F12.
*    伝票枚数
     MOVE      HEN-F07             TO  WK-FIL-F13.
*    予備
     MOVE      SPACE               TO  WK-FIL-F14.
*
 FILE-HEAD-EXIT.
     EXIT.
****************************************************************
*　　明細ヘッダ作成
****************************************************************
 MEISAI-HEAD-SEC       SECTION.
*
     MOVE     "MEISAI-HEAD-SEC"    TO  S-NAME.
*
     MOVE      SPACE               TO  WK-HED-REC.
     INITIALIZE                        WK-HED-REC.
*    レコード区分
     MOVE      "B"                 TO  WK-HED-F01.
*    データ種別
*****MOVE      "51"                TO  WK-HED-F02.
     EVALUATE  HEN-F092
         WHEN   22   MOVE   "62"   TO  WK-HED-F02
         WHEN   23   MOVE   "63"   TO  WK-HED-F02
         WHEN   24   MOVE   "64"   TO  WK-HED-F02
         WHEN   25   MOVE   "65"   TO  WK-HED-F02
         WHEN   26   MOVE   "66"   TO  WK-HED-F02
         WHEN  OTHER MOVE   "62"   TO  WK-HED-F02
     END-EVALUATE.
*    取引先ＣＤ
     MOVE      HEN-F093            TO  WK-HED-F03.
*    法人ＣＤ
     MOVE      HEN-F094            TO  WK-HED-F04.
*    店コード
     MOVE      HEN-F095            TO  WK-HED-F05.
*    発注種別
     MOVE      ZERO                TO  WK-HED-F06.
*    伝票区分
     MOVE      HEN-F099            TO  WK-HED-F07.
*    発注日
     MOVE      HEN-F09A            TO  WK-HED-F08.
*    納品伝票番号
     MOVE      HEN-F096            TO  WK-HED-F09.
*    部門
     MOVE      HEN-F098            TO  WK-HED-F10.
*    特売ＣＤ
     MOVE      HEN-F09K            TO  WK-HED-F11.
*    館番号
     MOVE      HEN-F09L            TO  WK-HED-F12.
*    岐南店対応 2003/04/17 NAV
*****IF        HEN-F095  =  214
*****          MOVE HEN-F09L(1:1)  TO  WK-HED-F12
*****END-IF.
*    申込票番号
     MOVE      SPACE               TO  WK-HED-F13.
*    仕入先略称
     MOVE      HEN-F09C            TO  WK-HED-F14.
*    社名
     MOVE      HEN-F09D            TO  WK-HED-F15.
*    店名
     MOVE      HEN-F09E            TO  WK-HED-F16.
*    予備
     MOVE      SPACE               TO  WK-HED-F17.
*    納品先店コード
     MOVE      HEN-F09M            TO  WK-HED-F18.
*    予備
     MOVE      SPACE               TO  WK-HED-F19.
*
 MEISAI-HEAD-EXIT.
     EXIT.
****************************************************************
*　　明細ボディー作成
****************************************************************
 MEISAI-BODY-SEC       SECTION.
*
     MOVE     "MEISAI-BODY-SEC"    TO  S-NAME.
*
     MOVE      SPACE               TO  WK-MEI-REC.
     INITIALIZE                        WK-MEI-REC.
*    レコード区分
     MOVE      "D"                 TO  WK-MEI-F01.
*    データ種別
*****MOVE      "51"                TO  WK-MEI-F02.
     EVALUATE  HEN-F092
         WHEN   22   MOVE   "62"   TO  WK-MEI-F02
         WHEN   23   MOVE   "63"   TO  WK-MEI-F02
         WHEN   24   MOVE   "64"   TO  WK-MEI-F02
         WHEN   25   MOVE   "65"   TO  WK-MEI-F02
         WHEN   26   MOVE   "66"   TO  WK-MEI-F02
         WHEN  OTHER MOVE   "62"   TO  WK-MEI-F02
     END-EVALUATE.
*    発注伝票番号
     MOVE      HEN-F096            TO  WK-MEI-F03.
*    発注伝票行番号
     MOVE      HEN-F105            TO  WK-MEI-F04.
*    納品伝票行番号
     MOVE      HEN-F05             TO  WK-MEI-F05.
*    納品日
     MOVE      HEN-F09B            TO  WK-MEI-F06.
*    検収日
     MOVE      ZERO                TO  WK-MEI-F07.
*    商品ＣＤ
     MOVE      HEN-F106            TO  WK-MEI-F08.
*    ＪＡＮＣＤ
     MOVE      HEN-F10A            TO  WK-MEI-F09.
*    商品種別
     MOVE      HEN-F107            TO  WK-MEI-F10.
*    品名
     MOVE      HEN-F108            TO  WK-MEI-F11.
*    規格
     MOVE      HEN-F109            TO  WK-MEI-F12.
*    原単価
     MOVE      HEN-F10C            TO  WK-MEI-F13.
*    売単価
     MOVE      HEN-F10D            TO  WK-MEI-F14.
*    発注数量
     MOVE      HEN-F10B            TO  WK-MEI-F15.
*    納品数量
     MOVE      HEN-F11             TO  WK-MEI-F16.
*    検品数量
     MOVE      ZERO                TO  WK-MEI-F17.
*    予備
     MOVE      SPACE               TO  WK-MEI-F18.
*
 FILE-HEAD-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
*    テーブル判定
***  IF       IDX  =  1
***           WRITE     SND-REC
***           ADD       1         TO   SND-CNT
***  END-IF.
*
     DISPLAY "ｶｰﾏ ｿｳｼﾝDT READ  CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｶｰﾏ ｿｳｼﾝDT WRITE CNT = " SND-CNT   UPON CONS.
*
     CLOSE     SNDKGSS  KGHENKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
