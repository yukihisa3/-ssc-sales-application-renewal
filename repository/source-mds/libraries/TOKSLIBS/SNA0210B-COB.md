# SNA0210B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0210B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携                    *
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　取消依頼データ作成                *
*    作成日／更新日　　　：　11/10/25                          *
*    作成者／更新者　　　：　ＮＡＶ畠山　                      *
*    処理概要　　　　　　：　パラメータより取消依頼データを作成*
*    作成日／更新日　　　：　12/07/17                          *
*    作成者／更新者　　　：　ＮＡＶ井上　                      *
*    処理概要　　　　　　：　ＯＵＴＰＵＴに項目追加            *
*                    　　：　小売連携区分、商品名１，２        *
*　　更新日／更新者　　　：　2012/09/04 NAV TAKAHASHI          *
*    修正概要　　　　　　：                                    *
*      取引先マスタより、計上部門コードを受取り、計上部門コード*
*      を連携データにセットする。　　　　　　　　　　　　　　　*
*　　　計上部門→Ｆ０２、所属部門→Ｆ３２にセット　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0210B.
 AUTHOR.               HATAKEYAMA.
 DATE-WRITTEN.         11/10/25.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*
*連携NO管理テーブル
     SELECT  NARKANL  ASSIGN TO    DA-01-VI-NARKANL1
             ORGANIZAITION         INDEXED
             ACCESS      MODE      RANDOM
             RECORD      KEY       NAK-F01
                         FILE      STATUS    NAK-ST.
*小売携累積ファイル
     SELECT  NARRUIF  ASSIGN TO    DA-01-VI-NARRUIL1
             ORGANIZAITION         INDEXED
             ACCESS      MODE      SEQUENTIAL
             RECORD      KEY       NAR-F01
                         FILE      STATUS    NAR-ST.
*小売連携発注データ
     SELECT  NARTORF  ASSIGN TO    DA-01-S-NARTORF
             ORGANIZAITION         SEQUENTIAL
                         FILE      STATUS    NAT-ST.
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 連携NO管理テーブル　　　　　　　　　             *
****************************************************************
 FD  NARKANL           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARKANF   OF   XFDLIB
                       JOINING   NAK       AS   PREFIX.
****************************************************************
*    FILE = 小売連携累積ファイル　　　　　　　　　             *
****************************************************************
 FD  NARRUIF           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARRUIF   OF   XFDLIB
                       JOINING   NAR       AS   PREFIX.
*
****************************************************************
*    FILE = 連携取消依頼データ                                *
****************************************************************
 FD  NARTORF           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARTORF   OF   XFDLIB
                       JOINING   NAT       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(01)     VALUE   SPACE.
 01  CHUSHUTU-FLG              PIC  X(01)     VALUE   SPACE.
 01  SEL-CNT                   PIC  9(06)     VALUE   ZERO.
 01  INIT-FLG                  PIC  X(01)     VALUE   SPACE.
 01  WK-RENKEI-NO              PIC  9(09)     VALUE   ZERO.
 01  WK-HANTEI                 PIC  X(01)     VALUE   SPACE.
 01  HZK.
     03  YMD01                 PIC  9(02)     VALUE   20.
     03  YMD02                 PIC  9(06)     VALUE   ZERO.
 01  JIKAN.
     03  JIKAN01               PIC  9(06)     VALUE   ZERO.
     03  JIKAN02               PIC  9(02)     VALUE   ZERO.
*ステータス領域
 01  STATUS-AREA.
     03  NAK-ST                PIC  X(02).
     03  NAR-ST                PIC  X(02).
     03  NAT-ST                PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  NAK-ERR           PIC N(13) VALUE
         NC"連携ＮＯ管理テーブルエラー".
     03  NAR-ERR           PIC N(13) VALUE
         NC"小売連携累積ファイルエラー".
     03  NAT-ERR           PIC N(12) VALUE
         NC"連携取消依頼データエラー".
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
*------------------------------------------------------------*
 01  MSG-AREA.
*----- 指定された連携NOが管理テーブルに存在しないメッセージ
*
     03  MSG-KENSU1.
         05  KENSU1A PIC N(13) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊＊".
         05  KENSU1B PIC N(13) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-KENSU2.
         05  KENSU2A PIC N(26) VALUE
      NC"＊指定された連携ＮＯは管理テーブルに存在しません。＊".
     03  MSG-KENSU3.
         05  KENSU3A PIC N(11) VALUE NC"＊　　　連携ＮＯ　＝　".
         05  RENKEI-NO-FROM PIC X(09) VALUE SPACE.
         05  KENSU3B PIC N(10) VALUE NC"　　　　　　　　　＊".
*----- 指定された連携NOで再抽出できない状態
     03  MSG-KENSU4.
         05  KENSU4A PIC N(26) VALUE
      NC"＊　指定された連携ＮＯは取消できない状態です　　　＊".
     03  MSG-KENSU5.
         05  KENSU5A PIC  N(10) VALUE NC"＊　　　　状態　＝　".
         05  JOUTAI-CD   PIC  9(01)  VALUE ZERO.
         05  JOUTAI-MEI  PIC  N(08)  VALUE SPACE.
         05  KENSU5B     PIC  N(05)  VALUE NC"　　　　＊".
*----- 指定された連携NOが累積ファイルに存在しない
     03  MSG-KENSU6.
         05  KENSU6A PIC N(26) VALUE
      NC"＊指定された連携ＮＯは累積ファイルに存在しません。＊".
*----- 再抽出件数の表示
     03  MSG-KENSU7.
         05  KENSU7A PIC N(26) VALUE
       NC"＊　取消し依頼データ抽出終了　　　　　　　　　　　＊".
     03  MSG-KENSU8.
         05  KENSU8A PIC N(10) VALUE
         NC"＊　　件数　　　＝　".
         05  CHUSHUTU-KENSUU  PIC Z(06) VALUE ZERO.
         05  KENSU8B PIC    N(13) VALUE
     NC"　件　　　　　　　　　　＊".
     03  MSG-KEIKOKU  PIC N(27) VALUE
     NC"指定ＮＯは取消依頼済です。再度依頼しますか？（Ｙ・Ｎ）".
*--- 状態表示
     03  JOUTAI1    PIC    N(08) VALUE NC"（データ抽出済）".
     03  JOUTAI2    PIC    N(08) VALUE NC"（取消済）".
 LINKAGE              SECTION.
* パラメーター（B) エリア
 01  PARA-BT.
   03  PARA-BUMON                 PIC  X(04).
   03  PARA-TANTOU                PIC  X(02).
*01  PARA-BUMON                   PIC  X(04).
*01  PARA-TANTOU                  PIC  X(02).
 01  PARA-SHUBETU                 PIC  X(01).
 01  PARA-RENKEI-NO-FROM          PIC  X(09).
 01  PARA-RENKEI-NO-TO            PIC  X(09).
 01  PARA-TANMATU-IP              PIC  X(15).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION   USING  PARA-BT
*                                        PARA-TANTOU
                                         PARA-SHUBETU
                                         PARA-RENKEI-NO-FROM
                                         PARA-RENKEI-NO-TO
                                         PARA-TANMATU-IP.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANL.
     MOVE        NAK-ST     TO       E-ST.
     MOVE        "NARKANL"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC2             SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARRUIF.
     MOVE        NAR-ST     TO       E-ST.
     MOVE        "NARRUIF"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC3             SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARTORF.
     MOVE        NAT-ST      TO       E-ST.
     MOVE        "NARTORF"   TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  THRU  MAIN-EXIT
               UNTIL  END-FLG = 1.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT        NARRUIF.
     OPEN      INPUT        NARKANL.
     OPEN      OUTPUT       NARTORF.
     ACCEPT    YMD02        FROM   DATE.
     ACCEPT    JIKAN        FROM   TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*-------連携ＮＯ管理テーブルの読み込み
 MAIN010.
     MOVE  PARA-RENKEI-NO-FROM   TO NAK-F01.
     READ  NARKANL
           INVALID
     PERFORM DSP-TAISHOUNASI1-SEC THRU DSP-TAISHOUNASI1-EXIT
           MOVE   "1"        TO   END-FLG
     END-READ.
 MAIN011.
     IF  NAK-F04  IS =  "2"  OR "3"
         MOVE     SPACE       TO   CHUSHUTU-FLG
         PERFORM CHUSHUTU-SEC   THRU CHUSHUTU-EXIT
             UNTIL  CHUSHUTU-FLG = "1"
     ELSE
         IF  NAK-F04  IS =  "4"
             PERFORM DSP-KEIKOKU-SEC  THRU DSP-KEIKOKU-EXIT
*------ACCEPT "Y" での続行処理-------------------------
             MOVE     SPACE       TO   CHUSHUTU-FLG
             PERFORM CHUSHUTU-SEC   THRU CHUSHUTU-EXIT
                     UNTIL  CHUSHUTU-FLG = "1"
         ELSE
*-------状態区分＝1(ﾃﾞｰﾀ抽出済） OR 5(取消済）----
             PERFORM DSP-JOUTAIIJOU-SEC  THRU DSP-JOUTAIIJOU-EXIT
         END-IF
     END-IF.
 MAIN-EXIT.
     EXIT.
**************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     IF     SEL-CNT  =  ZERO
                        GO  TO       END-EXIT.
*------抽出件数の表示
     MOVE      SEL-CNT               TO  CHUSHUTU-KENSUU.
     MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
*
     DISPLAY   KENSU1A KENSU1B       UPON  CONS.
     DISPLAY   KENSU7A               UPON  CONS.
     DISPLAY   KENSU3A RENKEI-NO-FROM KENSU3B       UPON  CONS.
     DISPLAY   KENSU8A CHUSHUTU-KENSUU KENSU8B      UPON  CONS.
     DISPLAY   KENSU1A KENSU1B       UPON  CONS.
*
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのCLOSE処理
     CLOSE     NARKANL   NARRUIF  NARTORF.
*
 END-EXIT.
     EXIT.
****************************************************************
*             取消依頼データ　出力処理
****************************************************************
 CHUSHUTU-SEC       SECTION.
     IF     INIT-FLG NOT = SPACE
           GO  TO  CHUSHUTU01.
*----初期処理-----------------------------------------------
     MOVE   "1"       TO    INIT-FLG.
     MOVE   NAK-F01   TO    NAR-F01.
     START  NARRUIF   KEY = NAR-F01
            INVALID
     PERFORM DSP-TAISHOUNASI2-SEC THRU DSP-TAISHOUNASI2-EXIT
            NOT INVALID
     MOVE   NAK-F01   TO    WK-RENKEI-NO
     END-START.
*
 CHUSHUTU01.
*
     READ     NARRUIF   AT  END
              MOVE     "1"   TO  END-FLG
              MOVE     "1"   TO  CHUSHUTU-FLG
              GO  TO  CHUSHUTU-EXIT
     END-READ.
     IF       NAR-F01  NOT = WK-RENKEI-NO
              MOVE     "1"   TO  END-FLG
              MOVE     "1"   TO  CHUSHUTU-FLG
              GO  TO   CHUSHUTU-EXIT
     ELSE
              PERFORM  TENSOU-SEC  THRU  TENSOU-EXIT
              WRITE    NAT-REC
              ADD      1  TO  SEL-CNT
     END-IF.
*
 CHUSHUTU-EXIT.
     EXIT.
**************************************************************
*        連携NO管理テーブルに対象データ無しを表示
**************************************************************
 DSP-TAISHOUNASI1-SEC SECTION.
*
     MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
     DISPLAY   KENSU1A KENSU1B       UPON  CONS.
     DISPLAY   KENSU2A               UPON  CONS.
     DISPLAY   KENSU3A RENKEI-NO-FROM KENSU3B       UPON  CONS.
     DISPLAY   KENSU1A KENSU1B       UPON  CONS.
     MOVE    "4001"    TO  PROGRAM-STATUS.
     STOP   RUN.
 DSP-TAISHOUNASI1-EXIT.
     EXIT.
**************************************************************
*        小売連携累積ファイルに対象データ無しを表示
**************************************************************
 DSP-TAISHOUNASI2-SEC SECTION.
*
     MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
     DISPLAY   KENSU1A KENSU1B       UPON  CONS.
     DISPLAY   KENSU6A               UPON  CONS.
     DISPLAY   KENSU3A RENKEI-NO-FROM KENSU3B       UPON  CONS.
     DISPLAY   KENSU1A KENSU1B       UPON  CONS.
     MOVE    "4001"    TO  PROGRAM-STATUS.
     STOP   RUN.
 DSP-TAISHOUNASI2-EXIT.
     EXIT.
**************************************************************
*            状態異常を表示
**************************************************************
 DSP-JOUTAIIJOU-SEC  SECTION.
*
     MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
     MOVE      NAK-F04               TO  JOUTAI-CD.
     EVALUATE  NAK-F04
               WHEN "1"
                    MOVE  JOUTAI1    TO  JOUTAI-MEI
               WHEN "5"
                    MOVE  JOUTAI2    TO  JOUTAI-MEI
      END-EVALUATE.
      DISPLAY   KENSU1A KENSU1B       UPON  CONS.
      DISPLAY   KENSU4A               UPON  CONS.
      DISPLAY   KENSU3A RENKEI-NO-FROM KENSU3B       UPON  CONS.
      DISPLAY   KENSU5A JOUTAI-CD JOUTAI-MEI KENSU5B UPON  CONS.
      DISPLAY   KENSU1A KENSU1B       UPON  CONS.
      MOVE    "4001"    TO   PROGRAM-STATUS.
      STOP   RUN.
 DSP-JOUTAIIJOU-EXIT.
      EXIT.
**************************************************************
*            警告を表示
**************************************************************
 DSP-KEIKOKU-SEC  SECTION.
*
     DISPLAY   MSG-KEIKOKU  UPON  CONS.
     ACCEPT    WK-HANTEI    FROM  CONS.
     IF       WK-HANTEI  =  "Y"    GO   TO  DSP-KEIKOKU-EXIT
     ELSE
     MOVE    "4001"    TO   PROGRAM-STATUS
     STOP   RUN
     END-IF.
*
 DSP-KEIKOKU-EXIT.
     EXIT.
**************************************************************
*            項目転送処理
**************************************************************
 TENSOU-SEC  SECTION.
*
     INITIALIZE              NAT-REC.
     MOVE    NAR-F01         TO  NAT-F01.
     MOVE    NAR-F02         TO  NAT-F02.
     MOVE    NAR-F03         TO  NAT-F03.
*----MOVE    NAR-F04         TO  NAT-F04.
     MOVE    NAR-F04         TO  NAT-F05.
*----MOVE    NAR-F05         TO  NAT-F05.
     MOVE    NAR-F05         TO  NAT-F04.
     MOVE    NAR-F06         TO  NAT-F06.
     MOVE    NAR-F07         TO  NAT-F07.
     MOVE    NAR-F08         TO  NAT-F08.
     MOVE    NAR-F09         TO  NAT-F09.
     MOVE    NAR-F10         TO  NAT-F10.
     MOVE    NAR-F11         TO  NAT-F11.
     MOVE    NAR-F12         TO  NAT-F12.
     MOVE    NAR-F13         TO  NAT-F13.
     MOVE    NAR-F14         TO  NAT-F14.
     MOVE    NAR-F15         TO  NAT-F15.
     MOVE    NAR-F16         TO  NAT-F16.
     MOVE    NAR-F17         TO  NAT-F17.
     MOVE    NAR-F18         TO  NAT-F18.
     MOVE    NAR-F19         TO  NAT-F19.
     MOVE    NAR-F20         TO  NAT-F20.
     MOVE    NAR-F21         TO  NAT-F21.
     MOVE    NAR-F22         TO  NAT-F22.
     MOVE    NAR-F23         TO  NAT-F23.
     MOVE    NAR-F24         TO  NAT-F24.
     MOVE    NAR-F25         TO  NAT-F25.
     MOVE    NAR-F26         TO  NAT-F26.
     MOVE    NAR-F27         TO  NAT-F27.
     MOVE    NAR-F28         TO  NAT-F28.
*↓20120717
     MOVE    NAR-F29         TO  NAT-F29.
     MOVE    NAR-F30         TO  NAT-F30.
     MOVE    NAR-F31         TO  NAT-F31.
*↑20120717
*↓20120904
     MOVE    NAR-F32         TO  NAT-F32.
*↑20120904
     MOVE    NAR-FIL01       TO  NAT-FIL01.
     MOVE    HZK             TO  NAT-F98.
     MOVE    JIKAN01         TO  NAT-F99.
     MOVE    "2"             TO  NAT-F97.
*
 TENSOU-EXIT.
     EXIT.

```
